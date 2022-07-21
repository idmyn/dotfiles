/* global Screen, Window, App, Key */

// inspiration:
// https://github.com/Jaredk3nt/phoenix-padding
// https://github.com/mafredri/phoenix-config/blob/5f4d7083d3e6dbc8956e63c2e010a05333e3af24/src/phoenix.ts#L29-L47

const pathToShell = pathToShellInNixStore || '/bin/sh' // pathToShellInNixStore is set by nix home-manager config

Phoenix.set({ 'daemon': true }) // hide menu bar icon

const currentScreen = () => Screen.main().flippedVisibleFrame()

const nextScreen = () => {
  const index =
    Screen.all().indexOf(Screen.main()) + 1 === Screen.all().length
      ? 0
      : Screen.all().indexOf(Screen.main()) + 1
  return Screen.all()[index].flippedVisibleFrame()
}

const windowLocations = {
  full: (scr) => ({
    y: scr.y,
    x: scr.x,
    width: scr.width,
    height: scr.height
  }),
  left: (scr) => ({
    y: scr.y,
    x: scr.x,
    width: scr.width / 2,
    height: scr.height
  }),
  right: (scr) => ({
    y: scr.y,
    x: scr.x + scr.width / 2,
    width: scr.width / 2,
    height: scr.height
  })
}

/* eslint-disable no-unused-vars */
const windowToFull = new Key('f', ['alt', 'ctrl'], () => {
  Window.focused().setFrame(windowLocations.full(currentScreen()))
})

const windowToLeft = new Key('j', ['alt', 'ctrl'], () => {
  Window.focused().setFrame(windowLocations.left(currentScreen()))
})

const windowToRight = new Key(';', ['alt', 'ctrl'], () => {
  Window.focused().setFrame(windowLocations.right(currentScreen()))
})

const windowToFullNextScreen = new Key('o', ['alt', 'ctrl'], () => {
  Window.focused().setFrame(windowLocations.full(nextScreen()))
})

const showOrOpenEmacs = new Key('e', ['alt', 'ctrl'], () => {
  const app = App.all().filter(app => /emacs/i.test(app.name()))[0]
  const appName = app ? app.name() : 'Emacs'
  if (App.get(appName)) {
    if (App.get(appName).windows().length > 0) {
      App.get(appName).focus()
    } else {
      Task.run(pathToShell, ['-c', 'emacsclient -nc'])
      App.get(appName).focus()
    }
  } else {
    Task.run(pathToShell, ['-c', 'emacs --daemon && emacsclient  -nc'])
  }
})

const showOrOpenVSCodium = new Key('v', ['alt', 'ctrl'], () => {
  if (App.get('VSCodium')) {
    App.get('VSCodium').focus()
  } else if (App.get('Code')) {
    App.get('Code').focus()
  } else {
    App.launch('Code')
  }
})

const showOrOpenDesignTool = new Key('d', ['alt', 'ctrl'], () => {
  if (App.get('Figma')) {
    App.get('Figma').focus()
  } else {
    Task.run('/bin/sh', ['-c', 'open -a "Figma"'])
  }
})

const showOrOpenDevBrowser = new Key('b', ['alt', 'ctrl'], () => {
  if (App.get('Polypane')) {
    App.get('Polypane').focus()
  } else {
    Task.run('/bin/sh', ['-c', 'open -a "Polypane"'])
  }
})

const showOrOpenWebBrowser = new Key('w', ['alt', 'ctrl'], () => {
  if (App.get('Arc')) {
    App.get('Arc').focus()
  } else if (App.get('Brave Browser')) {
    App.get('Brave Browser').focus()
  } else if (App.get('Firefox Developer Edition')) {
    App.get('Firefox Developer Edition').focus()
  } else if (App.get('Firefox')) {
    App.get('Firefox').focus()
  } else if (App.get('Vivaldi')) {
    App.get('Vivaldi').focus()
  } else {
    Task.run('/bin/sh', ['-c', 'open -a "Arc"'])
  }
})

const showOrOpenTerminal = new Key('s', ['alt', 'ctrl'], () => {
  Task.run('/bin/sh', ['-c', 'open -a kitty'])
})

const showOrOpenMail = new Key('m', ['alt', 'ctrl'], () => {
  if (App.get('Microsoft Outlook')) {
    App.get('Microsoft Outlook').focus()
  } else {
    App.launch('Microsoft Outlook')
    setTimeout(() => App.get('Microsoft Outlook').focus(), 100)
  }
})

const showOrOpenLogseq = new Key('l', ['alt', 'ctrl'], () => {
  if (App.get('Logseq')) {
    App.get('Logseq').focus()
  } else {
    App.launch('Logseq')
  }
})

const showOrOpenChat = new Key('c', ['alt', 'ctrl'], () => {
  if (App.get('Slack')) {
    App.get('Slack').focus()
  } else {
    if (App.get('Telegram')) {
      App.get('Telegram').focus()
    } else {
      App.launch('Telegram')
    }
  }
})

const showOrOpenInsomnia = new Key('i', ['alt', 'ctrl'], () => {
  if (App.get('Insomnia')) {
    App.get('Insomnia').focus()
  } else {
    App.launch('Insomnia')
  }
})

const showOrOpenPostman = new Key('h', ['alt', 'ctrl'], () => {
  if (App.get('Postman')) {
    App.get('Postman').focus()
  } else {
    App.launch('Postman')
  }
})

// log stream --process Phoenix
 Phoenix.log(App.all().filter(app => /emacs/i.test(app.name())).map(app => app.name() + "\n"))
/* eslint-enable no-unused-vars */
