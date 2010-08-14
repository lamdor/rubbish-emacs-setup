
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

namespace :submodules do
  desc "Pull/merge git submodules"
  task :pull do
    submodules = `git submodule`.split("\n").map { |l| l.split[1] }
    submodules.each do |sm|

      Dir.chdir(sm) do
        puts "####"
        puts "#### IN #{sm}, please do your magic (pulling/merging) and then exit the shell"
        puts "####"

        ENV['NO_CDPATH'] = "1"
        system "git remote update"
        system "/bin/bash -i"
      end

      system "git add #{sm}"
      system "git commit -m 'updated #{sm}'"
    end

    puts "Done updating submodules"
  end

  desc "update submodules"
  task :update do
    system "git submodule update --init --recursive"
  end
end



task :default => [:clean_elc, :compile_el]
