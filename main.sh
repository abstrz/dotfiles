handle(){
  for folder in $HOME/dotfiles/src/*; do
    source $folder/data &&
      setopt globdots   &&
      for file_path in $folder/*; do 
        file=`basename $file_path`
        if [[ "$file" != "data" ]]; then 
          display_name="`basename $folder`/`basename $file_path`"

          if [[ "$1" == "-sf" ]] || [[ ! -e "$target_path/$file" ]]; then 
            echo "Do you want to link $display_name y/n (n is default, q to quit)?"
            read option
            if [[ "$option" == "q" ]]; then 
              echo "Quitting..."
              return -1
            fi
            if [[ "$option" == "y" ]]; then 
              if [[ ! -d "$target_path" ]]; then
                echo "Creating $target_path..." &&
                  mkdir $target_path > /dev/null 
              fi
              echo "Symbolically linking $display_name..."  &&
                doas ln $1 $file_path $target_path > /dev/null
            fi
          fi
        fi
      done
    done
  }
dotfiles(){
  echo "Overwrite existing dotfiles? (y/n, or q to quit)"
  read input 

  if [[ $input == q || $input == quit || $input == Q || $input == Quit ]]; then 
    echo "Quitting..."
    return -1 
  fi 
  if [[ $input == y || $input == yes || $input == Y || $input == Yes ]]; then 
    handle "-sf";
    return 1
  elif [[ $input == n || $input == no || $input == N || $input == No ]]; then 
    handle "-s";
    return 1
  else
    echo "Sorry, unrecognized option" 
    return 1
  fi
}

