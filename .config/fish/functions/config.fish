function config --wraps='git --git-dir=/home/aquabeam/.dotfiles --work-tree=/home/aquabeam' --description 'alias config=git --git-dir=/home/aquabeam/.dotfiles --work-tree=/home/aquabeam'
  git --git-dir=/home/aquabeam/.dotfiles --work-tree=/home/aquabeam $argv; 
end
