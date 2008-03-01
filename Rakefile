#! ruby
require 'fileutils'

ENV['CXX'] = "gcc"

task :woof => [:compile_to_c, :link_woof] do
  Dir.chdir("vm"){
    FileUtils.mv "woof.exe", "../"
  }
end

task :compile_to_c => [] do
  Dir.chdir("vm"){
    system "gsc -link lib/sc2 lib/comlist lib/srfi-1 lib/vector lib/string lib/port-copying lib/test compiler woof-object machine-state continuations exception-handling function-calling instructions primitives machine main"
  }
end

task :link_woof => [] do
  Dir.chdir("vm"){
    system "gcc -Wall -W -Wno-unused -O1 -fno-math-errno -fschedule-insns2 -fno-trapping-math -fno-strict-aliasing -fwrapv -fexpensive-optimizations -fforce-addr -fpeephole2 -falign-jumps -falign-functions -fno-function-cse -fregmove -fgcse-las -freorder-functions -fcaller-saves -fno-if-conversion2 -foptimize-sibling-calls -fcse-skip-blocks -funit-at-a-time -finline-functions -fno-common -mieee-fp -mno-cygwin -I/cygdrive/c/Gambit-C/4.0b20/include -I/cygdrive/usr/include -L/cygdrive/c/Gambit-C/4.0b20/lib -L/cygdrive/lib -L/cygdrive/lib/w32api  sc2.c comlist.c srfi-1.c vector.c string.c port-copying.c test.c compiler.c woof-object.c machine-state.c continuations.c exception-handling.c function-calling.c instructions.c primitives.c machine.c main.c main_.c -luser32 -lm -ldl -lutil -lgambc -lws2_32 -o woof.exe"
  }
end


task :woofc => [] do
  Dir.chdir("compiler"){
    system "alex woof_lex.x"
    system "ghc --make -package parsec -package mtl woof_lex.hs woof_parse.hs woof_main.hs woof_to_bc.hs -o woofc"
    FileUtils.mv "woofc.exe", "../"
  }
end

task :wooft => [] do
  Dir.chdir("compiler"){
    system "alex woof_lex.x"
    system "ghc --make -package parsec -package mtl woof_lex.hs woof_parse.hs woof_main_tree.hs woof_to_dot.hs -o wooft"
    FileUtils.mv "wooft.exe", "../"
  }
end

task :test_vm => [] do
  Dir.chdir("vm"){
    system "gsi test-all.scm"
  }
end

task :test_woof => [] do
  system "woof.exe kernel/test_all.woof"
end

task :test_all => [:test_vm, :test_woof] do
end

# Visualize the program structure
# Usage: rake gen_tree input.woof
task :gen_tree => [] do
  system "wooft.exe < #{ARGV[1]} | dot -Tsvg -o #{File.basename(ARGV[1])}.svg"
end






