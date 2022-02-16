function config --wraps='git --git-dir=/home/aquabeam/.dotfiles --work-tree=/home/aquabeam' --wraps='git --git-dir=$HOME/.dotfiles --work-tree=$HOME' --description 'alias config git --git-dir=/home/aquabeam/.dotfiles --work-tree=/home/aquabeam'
  git --git-dir=$HOME/.dotfiles --work-tree=$HOME $argv; 
end
