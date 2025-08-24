package scala.meta.pc;

import java.util.List;

public interface CompileResult {
    List<String> diagnostics();
    String fullTree();
}
