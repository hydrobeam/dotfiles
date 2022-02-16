function config --wraps='git --git-dir=/home/aquabeam/.dotfiles --work-tree=/home/aquabeam' --wraps='git --git-dir=$HOME/.dotfiles --work-tree=$HOME' --description 'config dotfiles setup'
  git --git-dir=/home/aquabeam/.dotfiles --work-tree=/home/aquabeam $argv; 
end
