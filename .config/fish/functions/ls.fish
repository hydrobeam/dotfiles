function ls --wraps='exa --icons -F --header' --description 'alias ls=exa --icons -F --header'
  exa --icons -F --header $argv; 
end
