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
  //const appName = 'emacs-28.0.50'
  const appName = 'Emacs'
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
  } else if (App.get('Visual Studio Code')) {
    App.get('Visual Studio Code').focus()
  } else {
    App.launch('Visual Studio Code')
  }
})

const showOrOpenSublime = new Key('t', ['alt', 'ctrl'], () => {
  if (App.get('Sublime Text')) {
    App.get('Sublime Text').focus()
  } else {
    App.launch('Sublime Text')
  }
})

const showOrOpenBrowser = new Key('w', ['alt', 'ctrl'], () => {
  if (App.get('Firefox Developer Edition')) {
    App.get('Firefox Developer Edition').focus()
  } else if (App.get('Firefox')) {
    App.get('Firefox').focus()
  } else if (App.get('Brave Browser')) {
    App.get('Brave Browser').focus()
  } else {
    // App.launch('Firefox') causes prompt for safe mode
    Task.run('/bin/sh', ['-c', 'open -a "Brave Browser"'])
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

const showOrOpenDash = new Key('d', ['alt', 'ctrl'], () => {
  if (App.get('Dash')) {
    App.get('Dash').focus()
  } else {
    App.launch('Dash')
  }
})

const showOrOpenNotes = new Key('n', ['alt', 'ctrl'], () => {
  if (App.get('WorkFlowy')) {
    App.get('WorkFlowy').focus()
  } else {
    App.launch('WorkFlowy')
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

const showOrOpenTodoist = new Key('t', ['alt', 'ctrl'], () => {
  if (App.get('Todoist')) {
    App.get('Todoist').focus()
  } else {
    App.launch('Todoist')
  }
})

//Phoenix.log(App.all().filter(app => /.*/i.test(app.name())).map(app => app.name() + "\n"))
/* eslint-enable no-unused-vars */
