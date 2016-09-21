source ~/.bash_profile

if [[ -n "${INSIDE_EMACS:-}" ]]; then
  export PAGER=cat
fi
