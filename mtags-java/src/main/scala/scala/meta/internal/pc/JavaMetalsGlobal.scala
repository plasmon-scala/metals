package scala.meta.internal.pc

import java.io.BufferedInputStream
import java.io.File
import java.io.InputStreamReader
import java.io.Writer
import java.lang.{Iterable => JIterable}
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.{util => ju}
import javax.lang.model.element.Element
import javax.lang.model.element.ExecutableElement
import javax.lang.model.element.PackageElement
import javax.lang.model.element.TypeElement
import javax.lang.model.element.TypeParameterElement
import javax.lang.model.element.VariableElement
import javax.lang.model.util.Elements
import javax.lang.model.util.Types
import javax.tools.Diagnostic
import javax.tools.DiagnosticListener
import javax.tools.JavaCompiler
import javax.tools.JavaFileManager
import javax.tools.JavaFileObject
import javax.tools.StandardLocation
import javax.tools.ToolProvider

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._
import scala.util.Using

import org.eclipse.lsp4j.Position
import scala.meta.internal.mtags.CommonMtagsEnrichments._
import scala.meta.internal.mtags.SourcePath
import scala.meta.pc.ContentType
import scala.meta.pc.OffsetParams
import scala.meta.pc.ParentSymbols
import scala.meta.pc.PresentationCompilerConfig
import scala.meta.pc.RangeParams
import scala.meta.pc.SymbolDocumentation
import scala.meta.pc.SymbolSearch

import com.sun.source.tree.CompilationUnitTree
import com.sun.source.tree.MethodTree
import com.sun.source.tree.Tree
import com.sun.source.tree.VariableTree
import com.sun.source.util.JavacTask
import com.sun.source.util.SourcePositions
import com.sun.source.util.TreePath
import com.sun.source.util.Trees

class JavaMetalsGlobal(
    moduleString: String,
    javaFileManager: JavaFileManager,
    val search: SymbolSearch,
    val metalsConfig: PresentationCompilerConfig,
    val classpath: Seq[Path],
    val logger: java.util.function.Consumer[String]
) {
  var lastVisitedParentTrees: List[TreePath] = Nil

  def positionFromParams(params: OffsetParams): CursorPosition = {
    params match {
      case p: RangeParams =>
        CursorPosition(p.offset(), p.offset(), p.endOffset())
      case p: OffsetParams => CursorPosition(p.offset(), p.offset(), p.offset())
    }
  }

  /**
   * Return the real start and end for the name. For definitions the start and end include the whole element.
   *
   * @param text
   * @param elementName
   * @param originalStart
   * @param originalEnd
   */
  def findIndentifierStartAndEnd(
      text: String,
      elementName: String,
      originalStart: Int,
      originalEnd: Int,
      leaf: Tree,
      root: CompilationUnitTree,
      sourcePositions: SourcePositions
  ): (Int, Int) =
    if (originalEnd - originalStart == elementName.length()) {
      (originalStart, originalEnd)
    } else {
      val declarationStart = leaf match {
        case mt: MethodTree =>
          sourcePositions.getEndPosition(root, mt.getReturnType())
        case vt: VariableTree =>
          sourcePositions.getEndPosition(root, vt.getType())
        case _ =>
          originalStart.toLong
      }
      val subText = text.substring(declarationStart.toInt, originalEnd)
      val nameIndex = subText.indexOf(elementName)
      if (nameIndex >= 0) {
        val nameStart = declarationStart + nameIndex
        val nameEnd = nameStart + elementName.length()
        (nameStart.toInt, nameEnd.toInt)
      } else {
        (originalStart, originalEnd)
      }
    }

  def compilerTreeNode(
      scanner: JavaTreeScanner,
      position: CursorPosition
  ): Option[TreePath] = {
    scanner.scan(scanner.root, position)
    lastVisitedParentTrees = scanner.lastVisitedParentTrees
    lastVisitedParentTrees.headOption
  }

  def compilationTask(sourceCode: String, uri: URI): JavacTask = {
    val javaFileObject = SourceJavaFileObject.make(sourceCode, uri)
    // kind of meh, but does the job
    val (bootClassPath, userClassPath) =
      classpath.partition(_.getFileName.toString == "rt.jar")
    val javaVersion = if (bootClassPath.isEmpty) "17" else "1.8"

    val javaVersionOptions =
      List("--source", javaVersion, "--target", javaVersion)
    val classPathOptions =
      if (bootClassPath.isEmpty)
        List("-classpath", classpath.mkString(File.pathSeparator))
      else
        List(
          s"-bootclasspath",
          s"${bootClassPath.mkString(File.pathSeparator)}",
          "-classpath",
          userClassPath.mkString(File.pathSeparator)
        )
    JavaMetalsGlobal.classpathCompilationTask(
      javaFileObject,
      None,
      javaVersionOptions ++ classPathOptions,
      javaFileManager
    )
  }

  def isAtIdentifier(
      treePath: TreePath,
      element: Element,
      text: String,
      offset: Int,
      trees: Trees,
      root: CompilationUnitTree
  ): Boolean = {
    val leaf = treePath.getLeaf()
    val sourcePositions = trees.getSourcePositions()
    val treeStart = sourcePositions.getStartPosition(root, leaf)
    val treeEnd = sourcePositions.getEndPosition(root, leaf)
    if (treeStart >= 0 && treeEnd >= 0) {
      val elementName = element.getSimpleName().toString()
      val (start, end) = findIndentifierStartAndEnd(
        text,
        elementName,
        treeStart.toInt,
        treeEnd.toInt,
        leaf,
        root,
        sourcePositions
      )
      start <= offset && end >= offset
    } else false
  }

  def offsetToPosition(offset: Int, text: String): Position = {
    var line = 0
    var character = 0
    var i = 0
    while (i < offset && i < text.length()) {
      if (text.charAt(i) == '\n') {
        line += 1
        character = 0
      } else {
        character += 1
      }
      i += 1
    }
    new Position(line, character)
  }

  def documentation(
      element: Element,
      types: Types,
      elements: Elements,
      contentType: ContentType
  ): Option[SymbolDocumentation] = {
    val sym = semanticdbSymbol(element)
    search
      .documentation(
        moduleString,
        sym,
        new ParentSymbols {
          override def parents(): java.util.List[String] = {
            element match {
              case executableElement: ExecutableElement =>
                element.getEnclosingElement match {
                  case enclosingElement: TypeElement =>
                    overriddenSymbols(
                      executableElement,
                      enclosingElement,
                      types,
                      elements
                    )
                  case _ => java.util.Collections.emptyList[String]
                }
              case _ => java.util.Collections.emptyList[String]
            }
          }
        },
        contentType,
        logger
      )
      .asScala
  }

  private def overriddenSymbols(
      executableElement: ExecutableElement,
      enclosingElement: TypeElement,
      types: Types,
      elements: Elements
  ): ju.List[String] = {
    val overriddenSymbols = for {
      // get superclasses
      superType <- types.directSupertypes(enclosingElement.asType()).asScala
      superElement = types.asElement(superType)
      // get elements of superclass
      enclosedElement <- superElement match {
        case typeElement: TypeElement =>
          typeElement.getEnclosedElements().asScala
        case _ => Nil
      }
      // filter out non-methods
      enclosedExecutableElement <- enclosedElement match {
        case enclosedExecutableElement: ExecutableElement =>
          Some(enclosedExecutableElement)
        case _ => None
      }
      // check super method overrides original method
      if (elements.overrides(
        executableElement,
        enclosedExecutableElement,
        enclosingElement
      ))
      symbol = semanticdbSymbol(enclosedExecutableElement)
    } yield symbol
    overriddenSymbols.toList.asJava
  }

  def semanticdbSymbol(element: Element): String = {

    @tailrec
    def descriptors(
        acc: List[Descriptor],
        element: Element
    ): List[Descriptor] = {
      if (element == null || element.getSimpleName.toString == "") {
        if (acc.isEmpty) Empty :: Nil
        else acc
      } else {
        val elements = {
          element match {
            case packageElement: PackageElement =>
              packageElement.getQualifiedName.toString
                .split('.')
                .map(Package(_))
                .toList
            case executableElement: ExecutableElement =>
              List(
                Method(
                  executableElement.getSimpleName().toString(),
                  disambiguator(executableElement)
                )
              )
            case typeElement: TypeElement =>
              List(Class(typeElement.getSimpleName().toString()))
            case typeParameterElement: TypeParameterElement =>
              List(
                TypeVariable(typeParameterElement.getSimpleName().toString())
              )
            case variableElement: VariableElement =>
              List(Var(variableElement.getSimpleName().toString()))
            case _ => List(Empty)
          }
        }

        descriptors(elements ::: acc, element.getEnclosingElement())
      }
    }

    val decs = descriptors(Nil, element).filter(_ != Empty)

    (decs match {
      case Nil => List.empty[Descriptor]
      case d @ (Package(_) :: _) => d
      case d => Package("_empty_") :: d
    }).mkString("")
  }

  private def disambiguator(executableElement: ExecutableElement): String = {
    val methods =
      executableElement.getEnclosingElement.getEnclosedElements.asScala
        .collect {
          case e: ExecutableElement
              if e.getSimpleName == executableElement.getSimpleName =>
            e
        }

    val index = methods.zipWithIndex.collectFirst {
      case (e, i) if e.equals(executableElement) => i
    }

    index match {
      case Some(i) => if (i == 0) "()" else s"(+$i)"
      case None => "()"
    }
  }

  object Symbols {
    val None: String = ""
    val RootPackage: String = "_root_/"
  }

}

case class ModuleLocation(
    moduleName: String,
    path: Path
) extends JavaFileManager.Location {
  override def getName(): String =
    toString
  override def isOutputLocation(): Boolean =
    false
}

case class MetalsJavaFileObject(
    name: String,
    lastModified: Long,
    path: Path,
    entryName: String,
    ctx: SourcePath.Context
) extends javax.tools.JavaFileObject {
  def getKind = javax.tools.JavaFileObject.Kind.CLASS

  def isNameCompatible(
      simpleName: String,
      kind: javax.tools.JavaFileObject.Kind
  ): Boolean =
    // not too sure what we should do here
    kind == javax.tools.JavaFileObject.Kind.CLASS
  def getAccessLevel(): javax.lang.model.element.Modifier = null
  def getNestingKind(): javax.lang.model.element.NestingKind = null

  private def actualEntryName =
    s"classes/$entryName"

  def getName(): String = name
  def toUri(): java.net.URI =
    new URI(s"jar:${path.toUri.toASCIIString}!/$actualEntryName")

  def getLastModified(): Long = lastModified

  def openInputStream(): java.io.InputStream = {
    val zf = ctx.get(path)
    val ent = zf.getEntry(actualEntryName)
    new BufferedInputStream(zf.getInputStream(ent))
  }
  def openReader(x$1: Boolean): java.io.Reader =
    new InputStreamReader(openInputStream())
  def getCharContent(ignoreEncodingErrors: Boolean): CharSequence = {
    // FIXME What about ignoreEncodingErrors?
    val bytes = openInputStream().readAllBytes()
    new String(bytes, StandardCharsets.UTF_8) // FIXME Charset?
  }

  def delete(): Boolean = ???
  def openWriter(): java.io.Writer = ???
  def openOutputStream(): java.io.OutputStream = ???
}

class CustomFileManager(
    javaHome: Path,
    underlying: javax.tools.JavaFileManager,
    ctx: SourcePath.Context
) extends ForwardingJavaFileManager(underlying) {

  private val modsCache = new ju.concurrent.ConcurrentHashMap[Path, JIterable[
    ju.Set[JavaFileManager.Location]
  ]]

  def resetCache(): Unit =
    modsCache.clear()

  override def listLocationsForModules(
      location: JavaFileManager.Location
  ): JIterable[ju.Set[JavaFileManager.Location]] = {
    val res = location match {
      case StandardLocation.SYSTEM_MODULES =>
        val valueOrNull = modsCache.get(javaHome)
        if (valueOrNull == null) {
          val jmodsDir = javaHome.resolve("jmods")
          val mods =
            if (Files.isDirectory(jmodsDir))
              Using.resource(Files.list(jmodsDir)) { list =>
                list
                  .iterator()
                  .asScala
                  .filter(_.toString.endsWith(".jmod"))
                  .filter(Files.isRegularFile(_))
                  .map(jmod =>
                    ModuleLocation(
                      jmod.getFileName.toString.stripSuffix(".jmod"),
                      jmod
                    ): JavaFileManager.Location
                  )
                  .toSet
              }
            else
              Set.empty[JavaFileManager.Location]
          val mods0 = List(mods.asJava).asJava
          val prevOrNull = modsCache.putIfAbsent(javaHome, mods0)
          if (prevOrNull == null) mods0
          else prevOrNull
        } else
          valueOrNull
      case _ =>
        super.listLocationsForModules(location)
    }
    // scribe.info(s"JavaFileManager.listLocationsForModules($location) = " + pprint.apply(res.asScala.map(_.asScala)))
    res
  }

  override def getJavaFileForInput(
      location: JavaFileManager.Location,
      className: String,
      kind: JavaFileObject.Kind
  ): JavaFileObject =
    (location, kind) match {
      case (modLoc: ModuleLocation, JavaFileObject.Kind.CLASS) =>
        val entryPath = "classes/" + className.replace('.', '/') + ".class"
        ctx.entries(modLoc.path)
          .find(_.pathInZip == entryPath)
          .map(ent => MetalsJavaFileObject(
            ent.pathInZip.stripPrefix("classes/"),
            ent.lastModified,
            modLoc.path,
            ent.pathInZip.stripPrefix("classes/"),
            ctx
          ))
          .orNull
      case _ =>
        super.getJavaFileForInput(location, className, kind)
    }

  override def getLocationForModule(
      location: JavaFileManager.Location,
      moduleName: String
  ): JavaFileManager.Location =
    location match {
      case StandardLocation.SYSTEM_MODULES =>
        listLocationsForModules(location).asScala
          .flatMap(_.asScala)
          .collectFirst { case modLoc: ModuleLocation if modLoc.moduleName == moduleName => modLoc }
          .orNull
      case _ =>
        super.getLocationForModule(location, moduleName)
  }

  override def list(
      location: JavaFileManager.Location,
      packageName: String,
      kinds: ju.Set[JavaFileObject.Kind],
      recurse: Boolean
  ): JIterable[JavaFileObject] = {
    val res: JIterable[JavaFileObject] = location match {
      case modLoc: ModuleLocation =>
        val packagePath =
          if (packageName.isEmpty) ""
          else packageName.replace('.', '/') + "/"
        val prefix = s"classes/$packagePath"
        ctx
          .entries(modLoc.path)
          .filter(ent => ent.pathInZip.startsWith(prefix))
          .filter(ent => ent.pathInZip.endsWith(".class"))
          .filter(ent => !ent.pathInZip.endsWith("/module-info.class") && ent.pathInZip != "classes/module-info.class")
          .filter(ent => recurse || !ent.pathInZip.stripPrefix(prefix).contains('/'))
          .map { ent =>
            MetalsJavaFileObject(
              ent.pathInZip.stripPrefix("classes/"),
              ent.lastModified,
              modLoc.path,
              ent.pathInZip.stripPrefix("classes/"),
              ctx
            ): javax.tools.JavaFileObject
          }
          .toList
          .asJava
      case _ =>
        val res0 = super.list(location, packageName, kinds, recurse)
        // scribe.info(s"JavaFileManager.list($location (${location.getClass}), $packageName, $kinds, $recurse) = " + pprint.apply(res0.asScala))
        res0
    }
    res
  }

  override def inferModuleName(location: JavaFileManager.Location): String = {
    val res = location match {
      case modLoc: ModuleLocation =>
        modLoc.moduleName
      case _ =>
        val res0 = super.inferModuleName(location)
        // scribe.info(s"JavaFileManager.inferModuleName($location) = " + pprint.apply(res0))
        res0
    }
    // scribe.info(s"inferModuleName($location) = " + pprint.apply(res))
    res
  }

  override def inferBinaryName(
      location: JavaFileManager.Location,
      fileObj: JavaFileObject
  ): String = {
    val res = (location, fileObj) match {
      case (_: ModuleLocation, obj: MetalsJavaFileObject)
          if obj.entryName.endsWith(".class") =>
        obj.entryName.stripSuffix(".class").split('/').mkString(".")
      case _ =>
        val res0 = super.inferBinaryName(location, fileObj)
        res0
    }
    // scribe.info(s"inferBinaryName($location, $fileObj) = " + pprint.apply(res))
    res
  }

}

object JavaMetalsGlobal {

  val COMPILER: JavaCompiler = ToolProvider.getSystemJavaCompiler()

  private val noopDiagnosticListener = new DiagnosticListener[JavaFileObject] {

    // ignore errors since presentation compiler will have a lot of transient ones
    override def report(diagnostic: Diagnostic[_ <: JavaFileObject]): Unit = ()
  }

  def makeFileObject(file: File): JavaFileObject = {
    val fileManager = COMPILER.getStandardFileManager(null, null, null)
    val files = fileManager.getJavaFileObjectsFromFiles(List(file).asJava)
    files.iterator().next()
  }

  def baseCompilationTask(
      javaFileManager: JavaFileManager,
      sourceCode: String,
      uri: URI
  ): JavacTask = {
    val javaFileObject = SourceJavaFileObject.make(sourceCode, uri)
    JavaMetalsGlobal.classpathCompilationTask(
      javaFileObject,
      None,
      Nil,
      javaFileManager
    )
  }
  def classpathCompilationTask(
      javaFileObject: JavaFileObject,
      out: Option[Writer],
      allOptions: List[String],
      fileManager: JavaFileManager
  ): JavacTask = {
    COMPILER
      .getTask(
        out.orNull,
        fileManager,
        noopDiagnosticListener, // NOPE
        allOptions.asJava,
        null,
        List(javaFileObject).asJava
      )
      .asInstanceOf[JavacTask]
  }

  def scanner(task: JavacTask): JavaTreeScanner = {
    val elems = task.parse()
    task.analyze()
    val root = elems.iterator().next()

    new JavaTreeScanner(task, root)
  }

}
