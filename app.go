package main

import (
	// "encoding/json"

	"embed"
	"fmt"

	"granular/models"
	// . "granular/models"
	"context"

	"github.com/NduatiK/beeep"
	"github.com/wailsapp/wails/v2/pkg/runtime"
)

// App struct
type App struct {
	ctx        context.Context
	migrations embed.FS
}

// NewApp creates a new App application struct
func NewApp(migrations embed.FS) *App {
	return &App{migrations: migrations}
}

// startup is called at application startup
func (a *App) startup(ctx context.Context) {
	// EnvironmentInfo
	env := runtime.Environment(ctx)

	models.SetupDB(env.BuildType, a.migrations)

	// Perform your setup here
	a.ctx = ctx
}

// domReady is called after front-end resources have been loaded
func (a App) domReady(ctx context.Context) {
	// dir, _ := assets.ReadDir("assets")
	// dir2, _ := dir[0].Info()
	// dir2.
	err := beeep.Notify("Granular", "Title", "Message body", "assets/information.png")
	if err != nil {
		panic(err)
	}

	// Add your action here
}

// beforeClose is called when the application is about to quit,
// either by clicking the window close button or calling runtime.Quit.
// Returning true will cause the application to continue, false will continue shutdown as normal.
func (a *App) beforeClose(ctx context.Context) (prevent bool) {
	return false
}

// shutdown is called at application termination
func (a *App) shutdown(ctx context.Context) {
	models.DB.RawQuery("VACUUM").Exec()
	models.DB.Close()
	// Perform your teardown here
}

// Greet returns a greeting for the given name
func (a *App) Greet(name string) string {
	return fmt.Sprintf("Hello %s, It's show time!", name)
}
