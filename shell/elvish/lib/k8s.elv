SHOW_PROMPT = $false

fn toggle-prompt {
  if $SHOW_PROMPT {
    SHOW_PROMPT = $false
    edit:rprompt = { put "" }
  } else {
    SHOW_PROMPT = $true
    CONTEXT = ?(kubectl config current-context 2>/dev/null)
    if $CONTEXT {
      echo $CONTEXT
    } else {
      edit:rprompt = { put "no k8s context" }
    }
  }
}