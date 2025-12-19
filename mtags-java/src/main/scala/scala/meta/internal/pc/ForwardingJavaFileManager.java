package scala.meta.internal.pc;

import javax.tools.JavaFileManager;

public class ForwardingJavaFileManager extends javax.tools.ForwardingJavaFileManager<JavaFileManager> {
  public ForwardingJavaFileManager(JavaFileManager underlying) {
    super(underlying);
  }
}
