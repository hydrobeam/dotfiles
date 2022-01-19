function tlmgr --wraps='/usr/share/texmf-dist/scripts/texlive/tlmgr.pl --usermode' --description 'alias tlmgr=/usr/share/texmf-dist/scripts/texlive/tlmgr.pl --usermode'
  /usr/share/texmf-dist/scripts/texlive/tlmgr.pl --usermode $argv; 
end
