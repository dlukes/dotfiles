function trash
  set trash_dir ~/.trash
  mkdir -p $trash_dir
  mv -t $trash_dir $argv
end
