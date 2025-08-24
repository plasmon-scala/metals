package scala.meta.pc;

import java.nio.file.Path;
import java.util.Iterator;
import java.util.Map;
import java.util.zip.ZipFile;

public interface SourcePathContext {
  ZipFile get(Path path);
  Iterator<Map.Entry<Path, String>> entries(Path path);
  Object actualContext();
}