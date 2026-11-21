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

import scala.meta.internal.metals.CtSym
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
import org.eclipse.lsp4j.Position

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
    // The JDK classes of .jmod / ct.sym files come from the file manager, as system modules
    val (bootClassPath, userClassPath) =
      classpath
        .filterNot(CustomFileManager.isJdkClassFile)
        .partition(_.getFileName.toString == "rt.jar")
    val javaVersion = if (bootClassPath.isEmpty) "17" else "1.8"

    val javaVersionOptions =
      List("--source", javaVersion, "--target", javaVersion)
    val classPathOptions =
      if (bootClassPath.isEmpty)
        List("-classpath", userClassPath.mkString(File.pathSeparator))
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

/**
 * A module of the JDK, whose classes are read from an archive
 *
 * @param prefixes
 *   the archive entry prefixes the classes of the module are under: `classes/`
 *   in `.jmod` files, `<releases>/<module>/` in ct.sym (see [[CtSym]])
 * @param classSuffix
 *   the suffix of class entries: `.class` in `.jmod` files, `.sig` in ct.sym
 */
case class ModuleLocation(
    moduleName: String,
    path: Path,
    prefixes: Seq[String],
    classSuffix: String
) extends JavaFileManager.Location {
  override def getName(): String =
    toString
  override def isOutputLocation(): Boolean =
    false
}

object ModuleLocation {
  def jmod(jmod: Path): ModuleLocation =
    ModuleLocation(
      jmod.getFileName.toString.stripSuffix(".jmod"),
      jmod,
      Seq("classes/"),
      ".class"
    )
  def ctSym(ctSym: Path): Seq[ModuleLocation] =
    CtSym
      .layout(ctSym)
      .modules
      .toVector
      .sortBy(_._1)
      .map { case (module, prefixes) =>
        ModuleLocation(module, ctSym, prefixes, CtSym.classSuffix)
      }
}

/**
 * @param entryName
 *   path of the class file relative to its module, like
 *   `java/lang/String.class` (also for ct.sym signatures)
 * @param actualEntryName
 *   the name of the entry in the archive, like
 *   `classes/java/lang/String.class` or `MNO/java.base/java/lang/String.sig`
 */
case class MetalsJavaFileObject(
    name: String,
    lastModified: Long,
    path: Path,
    entryName: String,
    actualEntryName: String,
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

/**
 * A file manager reading the JDK system modules from `jdkClassFiles`, rather
 * than from the JDK javac runs on
 *
 * @param jdkClassFiles
 *   the `.jmod` files of the JDK, or its `lib/ct.sym` file (whose class
 *   signatures for the JDK's own release are used then)
 */
class CustomFileManager(
    jdkClassFiles: Seq[Path],
    underlying: javax.tools.JavaFileManager,
    ctx: SourcePath.Context
) extends ForwardingJavaFileManager(underlying) {

  /** Reads the `.jmod` files of `javaHome` */
  def this(
      javaHome: Path,
      underlying: javax.tools.JavaFileManager,
      ctx: SourcePath.Context
  ) =
    this(CustomFileManager.jmodFiles(javaHome), underlying, ctx)

  @volatile private var modsCache: JIterable[ju.Set[JavaFileManager.Location]] =
    null

  private val moduleEntriesCache =
    new ju.concurrent.ConcurrentHashMap[ModuleLocation, Vector[
      CustomFileManager.ModuleEntry
    ]]

  def resetCache(): Unit = {
    modsCache = null
    moduleEntriesCache.clear()
  }

  private def systemModules: JIterable[ju.Set[JavaFileManager.Location]] = {
    val valueOrNull = modsCache
    if (valueOrNull == null) {
      val mods = jdkClassFiles
        .filter(Files.isRegularFile(_))
        .flatMap { f =>
          if (CtSym.isCtSym(f)) ModuleLocation.ctSym(f)
          else if (f.getFileName.toString.endsWith(".jmod"))
            Seq(ModuleLocation.jmod(f))
          else Nil
        }
        .map(loc => loc: JavaFileManager.Location)
        .toSet
      val mods0 = List(mods.asJava).asJava
      modsCache = mods0
      mods0
    } else
      valueOrNull
  }

  private def moduleEntries(
      modLoc: ModuleLocation
  ): Vector[CustomFileManager.ModuleEntry] = {
    val valueOrNull = moduleEntriesCache.get(modLoc)
    if (valueOrNull == null) {
      val entries = ctx
        .entries(modLoc.path)
        .flatMap { ent =>
          val name = ent.pathInZip
          if (name.endsWith(modLoc.classSuffix))
            modLoc.prefixes.collectFirst {
              case prefix if name.startsWith(prefix) =>
                CustomFileManager.ModuleEntry(
                  name
                    .substring(prefix.length)
                    .stripSuffix(modLoc.classSuffix) + ".class",
                  name,
                  ent.lastModified
                )
            }
          else None
        }
        .toVector
      val previousOrNull = moduleEntriesCache.putIfAbsent(modLoc, entries)
      if (previousOrNull == null) entries
      else previousOrNull
    } else
      valueOrNull
  }

  private def fileObject(
      modLoc: ModuleLocation,
      entry: CustomFileManager.ModuleEntry
  ): MetalsJavaFileObject =
    MetalsJavaFileObject(
      entry.relPath,
      entry.lastModified,
      modLoc.path,
      entry.relPath,
      entry.entryName,
      ctx
    )

  override def listLocationsForModules(
      location: JavaFileManager.Location
  ): JIterable[ju.Set[JavaFileManager.Location]] =
    location match {
      case StandardLocation.SYSTEM_MODULES =>
        systemModules
      case _ =>
        super.listLocationsForModules(location)
    }

  override def getJavaFileForInput(
      location: JavaFileManager.Location,
      className: String,
      kind: JavaFileObject.Kind
  ): JavaFileObject =
    (location, kind) match {
      case (modLoc: ModuleLocation, JavaFileObject.Kind.CLASS) =>
        val relPath = className.replace('.', '/') + ".class"
        moduleEntries(modLoc)
          .find(_.relPath == relPath)
          .map(fileObject(modLoc, _))
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
          .collectFirst {
            case modLoc: ModuleLocation if modLoc.moduleName == moduleName =>
              modLoc
          }
          .orNull
      case _ =>
        super.getLocationForModule(location, moduleName)
    }

  override def list(
      location: JavaFileManager.Location,
      packageName: String,
      kinds: ju.Set[JavaFileObject.Kind],
      recurse: Boolean
  ): JIterable[JavaFileObject] =
    location match {
      case modLoc: ModuleLocation =>
        val prefix =
          if (packageName.isEmpty) ""
          else packageName.replace('.', '/') + "/"
        moduleEntries(modLoc)
          .filter(ent => ent.relPath.startsWith(prefix))
          .filter(ent =>
            !ent.relPath.endsWith(
              "/module-info.class"
            ) && ent.relPath != "module-info.class"
          )
          .filter(ent =>
            recurse || !ent.relPath.substring(prefix.length).contains('/')
          )
          .map(fileObject(modLoc, _): javax.tools.JavaFileObject)
          .toList
          .asJava
      case _ =>
        super.list(location, packageName, kinds, recurse)
    }

  override def inferModuleName(location: JavaFileManager.Location): String =
    location match {
      case modLoc: ModuleLocation =>
        modLoc.moduleName
      case _ =>
        super.inferModuleName(location)
    }

  override def inferBinaryName(
      location: JavaFileManager.Location,
      fileObj: JavaFileObject
  ): String =
    (location, fileObj) match {
      case (_: ModuleLocation, obj: MetalsJavaFileObject)
          if obj.entryName.endsWith(".class") =>
        obj.entryName.stripSuffix(".class").split('/').mkString(".")
      case _ =>
        super.inferBinaryName(location, fileObj)
    }

}

object CustomFileManager {

  /**
   * @param relPath
   *   path of the class relative to its module, like `java/lang/String.class`
   * @param entryName
   *   name of the archive entry
   */
  final case class ModuleEntry(
      relPath: String,
      entryName: String,
      lastModified: Long
  )

  def jmodFiles(javaHome: Path): Seq[Path] = {
    val jmodsDir = javaHome.resolve("jmods")
    if (Files.isDirectory(jmodsDir))
      Using.resource(Files.list(jmodsDir)) { list =>
        list
          .iterator()
          .asScala
          .filter(_.toString.endsWith(".jmod"))
          .filter(Files.isRegularFile(_))
          .toVector
      }
    else
      Nil
  }

  /** Whether a class path entry is one of the JDK files the file manager reads */
  def isJdkClassFile(path: Path): Boolean =
    CtSym.isCtSym(path) || path.getFileName.toString.endsWith(".jmod")
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
