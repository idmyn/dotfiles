SHOW_PROMPT = $false

fn toggle-prompt {
  if $SHOW_PROMPT {
    SHOW_PROMPT = $false
    edit:rprompt = { put "" }
  } else {
    SHOW_PROMPT = $true
    HAS_CONTEXT = ?(kubectl config current-context 1>&- 2>&-)
    if $HAS_CONTEXT {
      edit:rprompt = { put (kubectl config current-context):(kubectl config view --minify --output 'jsonpath={..namespace}') }
    } else {
      edit:rprompt = { put "no context" }
    }
  }
}
