task :default => [:compile]

task :ascii_helpers do
  `ghc -c AsciiHelpers.hs`
end

task :life => [:ascii_helpers] do
  `ghc -c Life.hs AsciiHelpers.o`
end

task :compile => [:life] do
  `ghc -o testlife testlife.hs Life.o AsciiHelpers.o`
end
