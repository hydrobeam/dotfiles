function cs --wraps='xclip -selection clipboard' --description 'alias cs=xclip -selection clipboard'
  xclip -selection clipboard $argv; 
end
