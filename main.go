package main

import (
	"embed"
	"log"

	"github.com/wailsapp/wails/v2/pkg/options/mac"

	"github.com/wailsapp/wails/v2"
	"github.com/wailsapp/wails/v2/pkg/logger"
	"github.com/wailsapp/wails/v2/pkg/options"
	"github.com/wailsapp/wails/v2/pkg/options/windows"
)

//go:embed frontend/dist/index.html
var index_html embed.FS

//go:embed frontend/dist
var assets embed.FS

//go:embed migrations
var migrations embed.FS

//go:embed build/appicon.png
var icon []byte

func main() {
	// Create an instance of the app structure
	app := NewApp(migrations)

	// Create application with options
	err := wails.Run(&options.App{
		Title:     "Granular",
		Width:     1000,
		Height:    700,
		MinWidth:  400,
		MinHeight: 600,
		// MaxWidth:          1280,
		// MaxHeight:         800,
		DisableResize:     false,
		Fullscreen:        false,
		Frameless:         false,
		StartHidden:       false,
		HideWindowOnClose: false,
		RGBA:              &options.RGBA{R: 240, G: 240, B: 240, A: 255},
		// RGBA:             &options.RGBA{R: 255, G: 255, B: 255, A: 100},
		// RGBA:             &options.RGBA{R: 255, G: 255, B: 255, A: 0},
		Assets:           assets,
		Menu:             nil,
		Logger:           nil,
		LogLevel:         logger.DEBUG,
		OnStartup:        app.startup,
		OnDomReady:       app.domReady,
		OnBeforeClose:    app.beforeClose,
		OnShutdown:       app.shutdown,
		WindowStartState: options.Normal,
		Bind: []interface{}{
			app,
		},
		// Windows platform specific options
		Windows: &windows.Options{
			WebviewIsTransparent: false,
			WindowIsTranslucent:  false,
			// WebviewIsTransparent: true,
			// WindowIsTranslucent:  true,
			DisableWindowIcon: false,
			// DisableFramelessWindowDecorations: false,
			WebviewUserDataPath: "",
			// CustomTheme: &windows.ThemeSettings{
			// 	// Theme to use when window is active
			// 	DarkModeTitleBar:   windows.RGB(255, 0, 0),   // Red
			// 	DarkModeTitleText:  windows.RGB(0, 255, 0),   // Green
			// 	DarkModeBorder:     windows.RGB(0, 0, 255),   // Blue
			// 	LightModeTitleBar:  windows.RGB(200, 200, 200),
			// 	LightModeTitleText: windows.RGB(20, 20, 20),
			// 	LightModeBorder:    windows.RGB(200, 200, 200),
			// 	// Theme to use when window is inactive
			// 	DarkModeTitleBarInactive:   windows.RGB(128, 0, 0),
			// 	DarkModeTitleTextInactive:  windows.RGB(0, 128, 0),
			// 	DarkModeBorderInactive:     windows.RGB(0, 0, 128),
			// 	LightModeTitleBarInactive:  windows.RGB(100, 100, 100),
			// 	LightModeTitleTextInactive: windows.RGB(10, 10, 10),
			// 	LightModeBorderInactive:    windows.RGB(100, 100, 100),
			// },
		},
		Mac: &mac.Options{
			TitleBar: &mac.TitleBar{
				TitlebarAppearsTransparent: true,
				HideTitle:                  false,
				HideTitleBar:               false,
				FullSizeContent:            false,
				UseToolbar:                 false,
				HideToolbarSeparator:       true,
			},
			Appearance:           mac.NSAppearanceNameDarkAqua,
			WebviewIsTransparent: true,
			WindowIsTranslucent:  true,
			About: &mac.AboutInfo{
				Title:   "Bookay",
				Message: "Book keeping",
				Icon:    icon,
			},
		},
	})

	if err != nil {
		log.Fatal(err)
	}

}
