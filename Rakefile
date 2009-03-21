namespace :emacs do 
  desc "compiles el files"
  task :compile_el do
    FileList["**/*.el"].each do |f|
      puts "compiling #{f}"
      system "emacs --batch -eval \"(batch-byte-compile-if-not-done)\" #{f} "
    end
  end
  
  desc "clean elc (compiled el) files"
  task :clean_elc do 
    FileList["**/*.elc"].each do |f|
      puts "removing #{f}"
      system "rm #{f}"
    end
  end
end


task :default => "emacs:compile_el"
