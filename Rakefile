EL_FILES = FileList["**/*.el"]

desc "compiles el files"
task :compile_el do
  EL_FILES.each do |f|
    system("emacs -batch -f batch-byte-compile #{f}")
  end
end

task :default => :compile_el
