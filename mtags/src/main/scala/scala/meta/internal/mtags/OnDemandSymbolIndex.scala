package scala.meta.internal.mtags

import java.util.zip.ZipError
import java.util.zip.ZipException

import scala.collection.concurrent.TrieMap
import scala.util.control.NonFatal

import scala.meta.Dialect
import scala.meta.dialects
import scala.meta.internal.io.{ListFiles => _}
import scala.meta.internal.metals.ScalaVersions
import scala.meta.internal.mtags.ScalametaCommonEnrichments._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.AbsolutePath
import scala.meta.pc.reports.ReportContext
import java.nio.file.Path
import scala.meta.inputs.Input

/**
 * An implementation of GlobalSymbolIndex with fast indexing and low memory usage.
 *
 * Fast indexing is enabled by ScalaToplevelMtags, a custom parser that extracts
 * only toplevel symbols from a Scala source file. Java source files don't need indexing
 * because their file location can be inferred from the symbol with the limitation
 * that it doesn't work for Java source files with multiple package-private top-level classes.
 *
 * Low memory usage is enabled by only storing "non-trivial toplevel" symbols.
 * A symbol is "toplevel" when its owner is a package. A symbol is "non-trivial"
 * when it doesn't match the path of the file it's defined in, for example `Some#`
 * in Option.scala is non-trivial while `Option#` in Option.scala is trivial.
 */
final class OnDemandSymbolIndex(
    dialectBuckets: TrieMap[
      (Dialect, GlobalSymbolIndex.Module),
      SymbolIndexBucket
    ],
    onError: PartialFunction[Throwable, Unit],
    sourceJars: () => OpenClassLoader,
    toIndexSource: AbsolutePath => AbsolutePath,
    javaHome: Path,
    onNewBucket: (SymbolIndexBucket, Dialect, GlobalSymbolIndex.Module) => Unit,
    val mtags: Mtags
)(implicit rc: ReportContext)
    extends GlobalSymbolIndex {
  // private lazy val sourceJars0 = sourceJars()
  var indexedSources = 0L
  def close(): Unit = {
    dialectBuckets.values.foreach(_.close())
  }
  private val onErrorOption = onError.andThen(_ => None)

  private def newRootBucket(): SymbolIndexBucket = {
    lazy val bucket: SymbolIndexBucket = SymbolIndexBucket.empty(
      scala.meta.dialects.Scala213Source3, // unused anyway
      mtags,
      sourceJars(),
      toIndexSource,
      onError,
      javaHome,
      javaOnly = true,
      addTextDocuments =
        (origin, path, docs) => addTextDocuments(bucket, origin, path, docs)
    )
    bucket
  }

  private var rootBucket = newRootBucket()

  def reset(module: GlobalSymbolIndex.Module): Unit =
    for (((dialect, module0), _) <- dialectBuckets.toList if module0 == module)
      dialectBuckets.remove((dialect, module))
  def clear(): Unit = {
    rootBucket = newRootBucket()
    dialectBuckets.clear()
  }

  private def getOrCreateBucket(
      dialect: Dialect,
      module: GlobalSymbolIndex.Module
  ): SymbolIndexBucket =
    dialectBuckets.getOrElseUpdate(
      (dialect, module), {
        val bucket = rootBucket.duplicate(dialect, javaOnly = false)
        onNewBucket(bucket, dialect, module)
        bucket
      }
    )

  override def definition(
      module: GlobalSymbolIndex.Module,
      symbol: Symbol
  ): Option[SymbolDefinition] = {
    try findSymbolDefinition(module, symbol).headOption
    catch {
      case NonFatal(e) =>
        onErrorOption(
          new IndexingExceptions.InvalidSymbolException(symbol.value, e)
        )
    }
  }

  override def definitions(
      module: GlobalSymbolIndex.Module,
      symbol: Symbol
  ): List[SymbolDefinition] =
    try findSymbolDefinition(module, symbol)
    catch {
      case NonFatal(e) =>
        onError(new IndexingExceptions.InvalidSymbolException(symbol.value, e))
        List.empty
    }

  override def addSourceDirectory(
      module: GlobalSymbolIndex.Module,
      dir: AbsolutePath,
      dialect: Dialect
  ): List[IndexingResult] =
    tryRun(
      dir.toString,
      List.empty,
      getOrCreateBucket(dialect, module).addSourceDirectory(module, dir)
    )

  // Traverses all source files in the given jar file and records
  // all non-trivial toplevel Scala symbols.
  override def addSourceJar(
      module: GlobalSymbolIndex.Module,
      jar: AbsolutePath,
      dialect: Dialect
  )(implicit ctx: SourcePath.Context): List[IndexingResult] =
    tryRun(
      jar.toString,
      List.empty, {
        try {
          getOrCreateBucket(dialect, module).addSourceJar(jar)
        } catch {
          case e: ZipError =>
            onError(new IndexingExceptions.InvalidJarException(jar, e))
            List.empty
          case e: ZipException =>
            onError(new IndexingExceptions.InvalidJarException(jar, e))
            List.empty
        }
      }
    )

  override def addSourceJar(
      jar: AbsolutePath
  )(implicit ctx: SourcePath.Context): List[IndexingResult] =
    tryRun(
      jar.toString,
      List.empty, {
        try {
          var res = rootBucket.addSourceJar(jar, isPureJava = true)
          for (bucket <- dialectBuckets.values) {
            res = bucket.addSourceJar(jar, isPureJava = true)
          }
          res
        } catch {
          case e: ZipError =>
            onError(new IndexingExceptions.InvalidJarException(jar, e))
            List.empty
          case e: ZipException =>
            onError(new IndexingExceptions.InvalidJarException(jar, e))
            List.empty
        }
      }
    )

  // Traverses all source files in the given jar file and returns
  // all non-trivial toplevel Scala symbols.
  def indexSource(
      module: GlobalSymbolIndex.Module,
      input: Input.VirtualFile,
      dialect: Dialect
  ): IndexingResult =
    getOrCreateBucket(dialect, module).indexSource(input, isJava = false)

  // Used to add cached toplevel symbols to index
  def addIndexedSourceJar(
      module: GlobalSymbolIndex.Module,
      jar: AbsolutePath,
      symbols: List[(String, SourcePath.ZipEntry)],
      dialect: Dialect
  ): Unit = {
    getOrCreateBucket(dialect, module).addIndexedSourceJar(jar, symbols)
  }

  def addToplevelSymbol(
      module: GlobalSymbolIndex.Module,
      path: String,
      source: SourcePath,
      toplevel: String,
      dialect: Dialect
  ): Unit =
    getOrCreateBucket(dialect, module).addToplevelSymbol(
      path,
      module,
      source,
      toplevel
    )

  private def tryRun[A](path: String, fallback: => A, thunk: => A): A =
    try thunk
    catch {
      case NonFatal(e) =>
        onError(new IndexingExceptions.PathIndexingException(path, e))
        fallback
    }

  private def findSymbolDefinition(
      module: GlobalSymbolIndex.Module,
      querySymbol: Symbol
  ): List[SymbolDefinition] = {
    module match {
      case s: GlobalSymbolIndex.Standalone =>
        val dialect = ScalaVersions.dialectForScalaVersion(
          s.scalaVersion,
          includeSource3 = true
        )
        getOrCreateBucket(dialect, module)
      case _: GlobalSymbolIndex.BuildTarget =>
    }
    dialectBuckets.toList
      .flatMap { case ((_, module0), bucket) =>
        if (module == module0)
          bucket.query(querySymbol)
        else
          Nil
      }
      // prioritize defs where found symbols is exact and comes from scala3
      .sortBy(d => (!d.isExact, d.dialect != dialects.Scala3))
  }

  def findFileForToplevel(
      topLevelSymbol: Symbol
  )(implicit ctx: SourcePath.Context): List[(SourcePath, Dialect)] = {
    dialectBuckets.values.flatMap(_.findFileForToplevel(topLevelSymbol)).toList
  }

  // Records all global symbol definitions.
  private def addTextDocuments(
      mainBucket: SymbolIndexBucket,
      originOpt: Option[Either[AbsolutePath, GlobalSymbolIndex.Module]],
      path: SourcePath,
      docs: s.TextDocuments
  ): Unit = {
    val originOpt0 = path match {
      case z: SourcePath.ZipEntry => Some(Left(z.zipPath))
      case _: SourcePath.Standard => originOpt.map(_.left.map(_.toNIO))
    }
    val buckets: Seq[SymbolIndexBucket] = originOpt0 match {
      case Some(Left(path)) =>
        dialectBuckets.valuesIterator
          .filter(_.sourceJars.hasEntry(path))
          .toVector
      case Some(Right(module)) => ???
      case None =>
        scribe.warn("???")
        Seq(mainBucket)
    }

    for {
      document <- docs.documents
      occ <- document.occurrences
      // we only care about global symbol definitions
      if occ.symbol.isGlobal && occ.role.isDefinition
      bucket <- buckets
    }
      bucket.definitions.updateWith(occ.symbol) {
        case Some(acc) => Some(acc + SymbolLocation(path, occ.range))
        case None => Some(Set(SymbolLocation(path, occ.range)))
      }
  }

}

object OnDemandSymbolIndex {

  def empty(
      javaHome: Path,
      mtags: Mtags,
      onError: PartialFunction[Throwable, Unit] = { case e: Throwable =>
        throw e
      },
      sourceJars: () => OpenClassLoader = () => new OpenClassLoader,
      toIndexSource: AbsolutePath => AbsolutePath = identity,
      onNewBucket: (
          SymbolIndexBucket,
          Dialect,
          GlobalSymbolIndex.Module
      ) => Unit = (_, _, _) => ()
  )(implicit rc: ReportContext): OnDemandSymbolIndex = {
    new OnDemandSymbolIndex(
      TrieMap.empty,
      onError,
      sourceJars,
      toIndexSource,
      javaHome,
      onNewBucket,
      mtags
    )
  }

}
