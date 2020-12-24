handle(){
  for folder in $PWD/src/*; do
    source $folder/init.sh &&
      shopt -s dotglob     &&
      for file_path in $folder/*; do 
        file=`basename $file_path`
        if [ "$file" != "init.sh" ]; then 
          display_name="`basename $folder`/`basename $file_path`"
          echo "Do you want to link $display_name (y/n, n is default)?"
          read skip
          if [ "$skip" == "y" ]; then 
            if [ "$1" == "-sf" ] || [ ! -e "$target_path/$file" ]; then 
              if [ ! -d "$target_path" ]; then
                echo "Creating $target_path..." &&
                  mkdir $target_path > /dev/null 
              fi
              echo "Symbolically linking $display_name..."  &&
                doas ln $1 $file_path $target_path > /dev/null
                            else 
                              echo "$target_path/$file exists. Skipping..."
            fi
          else 
            echo "Skipping..."
          fi
        fi
      done
    done
  }
main(){
  echo "Overwrite existing dotfiles? (y/n, or q to quit)"
  read input 

  if [[ $input == q || $input == quit || $input == Q || $input == Quit ]]; then 
    echo "Quitting..."
    exit -1 
  fi 
  if [[ $input == y || $input == yes || $input == Y || $input == Yes ]]; then 
    handle "-sf"
    exit 1
  elif [[ $input == n || $input == no || $input == N || $input == No ]]; then 
    handle "-s"
    exit 1
  else
    echo "Sorry, unrecognized option" 
    exit 1
  fi
}

main

