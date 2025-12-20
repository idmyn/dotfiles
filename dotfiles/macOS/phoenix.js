/* global Screen, Window, App, Key */

// inspiration:
// https://github.com/Jaredk3nt/phoenix-padding
// https://github.com/mafredri/phoenix-config/blob/5f4d7083d3e6dbc8956e63c2e010a05333e3af24/src/phoenix.ts#L29-L47

const pathToShell = pathToShellInNixStore || "/bin/sh"; // pathToShellInNixStore is set by nix home-manager config

Phoenix.set({ daemon: true }); // hide menu bar icon

const currentScreen = () => Screen.main().flippedVisibleFrame();

const nextScreen = () => {
  const index =
    Screen.all().indexOf(Screen.main()) + 1 === Screen.all().length
      ? 0
      : Screen.all().indexOf(Screen.main()) + 1;
  return Screen.all()[index].flippedVisibleFrame();
};

const windowLocations = {
  full: (scr) => ({
    y: scr.y,
    x: scr.x,
    width: scr.width,
    height: scr.height,
  }),
  left: (scr) => ({
    y: scr.y,
    x: scr.x,
    width: scr.width / 2,
    height: scr.height,
  }),
  right: (scr) => ({
    y: scr.y,
    x: scr.x + scr.width / 2,
    width: scr.width / 2,
    height: scr.height,
  }),
  top: (scr) => ({
    y: scr.y,
    x: scr.x,
    width: scr.width,
    height: scr.height / 2,
  }),
  bottom: (scr) => ({
    y: scr.y + scr.height / 2,
    x: scr.x,
    width: scr.width,
    height: scr.height / 2,
  }),
  soloFloat: (scr) => ({
    y: scr.y + scr.height * 0.125,
    x: scr.x + scr.width * 0.125,
    width: scr.width * 0.75,
    height: scr.height * 0.75,
  }),
};

const isDualUpMonitor = () => {
  const { width, height } = currentScreen();
  return height > width;
};

const isAlreadyFullscreen = (window) => {
  const { width: windowWidth, height: windowHeight } = window.frame();
  const { width: screenWidth, height: screenHeight } = currentScreen();

  return windowWidth === screenWidth && windowHeight === screenHeight;
};

/* eslint-disable no-unused-vars */
const windowToFull = new Key("f", ["alt", "ctrl"], () => {
  if (isDualUpMonitor() && isAlreadyFullscreen(Window.focused())) {
    Window.focused()
      .others({ visible: true })
      .forEach((w) => w.minimise());
    Window.focused().setFrame(windowLocations.soloFloat(currentScreen()));
  } else {
    Window.focused().setFrame(windowLocations.full(currentScreen()));
  }
});

const windowToLeft = new Key("h", ["alt", "ctrl"], () => {
  Window.focused().setFrame(windowLocations.left(currentScreen()));
});

const windowToRight = new Key("l", ["alt", "ctrl"], () => {
  Window.focused().setFrame(windowLocations.right(currentScreen()));
});

const windowToTop = new Key("k", ["alt", "ctrl"], () => {
  Window.focused().setFrame(windowLocations.top(currentScreen()));
});

const windowToBottom = new Key("j", ["alt", "ctrl"], () => {
  Window.focused().setFrame(windowLocations.bottom(currentScreen()));
});

const windowToFullNextScreen = new Key("o", ["alt", "ctrl"], () => {
  Window.focused().setFrame(windowLocations.full(nextScreen()));
});

const focusOrLaunch = (appName) => {
  if (App.get(appName)) {
    App.get(appName).focus();
  } else {
    App.launch(appName);
  }
};

//const showOrOpenThings = new Key('t', ['alt', 'ctrl'], () => focusOrLaunch('Things'))

const showOrOpenGitButler = new Key("g", ["alt", "ctrl"], () =>
  focusOrLaunch("GitButler"),
);

//const showOrOpenTana = new Key("t", ["alt", "ctrl"], () => {
//  focusOrLaunch("Tana");
//});

//const showOrOpenRaycast = new Key("r", ["alt", "ctrl"], () => {
//  Task.run("/bin/sh", ["-c", "open -a Raycast"]);
//});

const showOrOpenDesignTool = new Key("d", ["alt", "ctrl"], () => {
  if (App.get("Figma")) {
    App.get("Figma").focus();
  } else {
    Task.run("/bin/sh", ["-c", 'open -a "Figma"']);
  }
});

const showOrOpenDevBrowser = new Key("b", ["alt", "ctrl"], () => {
  if (App.get("Sizzy")) {
    App.get("Sizzy").focus();
  } else {
    Task.run("/bin/sh", ["-c", 'open -a "Sizzy"']);
  }
});

const showOrOpenNotes = new Key("n", ["alt", "ctrl"], () => {
  if (App.get("NotePlan")) {
    App.get("NotePlan").focus();
  } else {
    Task.run("/bin/sh", ["-c", 'open -a "NotePlan"']);
  }
});

const showOrOpen = (appName, { preferIfOpen = [] } = {}) => {
  const mainApp = App.get(appName);

  if (preferIfOpen.length > 0) {
    const firstOpenApp = preferIfOpen.find((app) => App.get(app));
    if (firstOpenApp) {
      const preferred = App.get(firstOpenApp);

      if (!preferred.isActive()) {
        preferred.focus();
        return;
      } else if (mainApp) {
        mainApp.focus();
      }
    }
  }

  if (mainApp) {
    mainApp.focus();
  } else {
    Task.run("/bin/sh", ["-c", `open -a "${appName}"`]);
  }
};

const showOrOpenWebBrowser = new Key("w", ["alt", "ctrl"], () => {
  showOrOpen("Helium", { preferIfOpen: ["Arc", "Google Chrome"] });
});

let lastUsedEditor;
const showOrOpenEditor = new Key("e", ["alt", "ctrl"], () => {
  //const zed = App.get("Zed");
  //const cursor = App.get("Cursor");
  //const openEditors = [zed, cursor].filter(Boolean);
  //if (openEditors.length === 2) {
  //  if (zed.isActive()) {
  //    cursor.focus();
  //    lastUsedEditor = cursor;
  //  } else if (cursor.isActive()) {
  //    zed.focus();
  //    lastUsedEditor = zed;
  //  } else if (lastUsedEditor) {
  //    lastUsedEditor.focus();
  //  } else {
  //    openEditors[0].focus();
  //    lastUsedEditor = openEditors[0];
  //  }
  //  return;
  //}
  showOrOpen("Zed", {preferIfOpen: ['Cursor']});
});

const showOrOpenZed = new Key("z", ["alt", "ctrl"], () => showOrOpen("Zed"));

const showOrOpenTerminal = new Key("s", ["alt", "ctrl"], () => {
  if (App.get("Ghostty")) {
    App.get("Ghostty").focus();
  } else {
    Task.run("/bin/sh", ["-c", "open -a ghostty"]);
  }
});

const showOrOpenMail = new Key("m", ["alt", "ctrl"], () => {
  if (App.get("Microsoft Outlook")) {
    App.get("Microsoft Outlook").focus();
  } else {
    App.launch("Microsoft Outlook");
    setTimeout(() => App.get("Microsoft Outlook").focus(), 100);
  }
});

const showOrOpenChat = new Key("c", ["alt", "ctrl"], () => {
  if (App.get("Slack")) {
    App.get("Slack").focus();
  } else {
    if (App.get("Telegram")) {
      App.get("Telegram").focus();
    } else {
      App.launch("Telegram");
    }
  }
});

const showOrOpenIaWriter = new Key("i", ["alt", "ctrl"], () => {
  focusOrLaunch("iA Writer");
});

// log stream --process Phoenix
Phoenix.log(
  App.all()
    .filter((app) => /code/i.test(app.name()))
    .map((app) => app.name() + "\n"),
);
/* eslint-enable no-unused-vars */
