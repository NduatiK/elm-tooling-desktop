package models

import (
	"embed"
	"fmt"
	"log"
	"os"

	"github.com/gobuffalo/pop/v6"
)

// DB is a connection to your database to be used
// throughout your application.
var DB *pop.Connection

func SetupDB(env string, migrations embed.FS) {
	var err error
	DB, err = pop.Connect(env)
	if err != nil {
		log.Fatal(err)
	}
	_ = pop.CreateDB(DB)
	var fm pop.FileMigrator

	fm, err = EmbeddedNewFileMigrator(DB, migrations)
	if err != nil {
		log.Fatal(err)
	} else {
		migrationErr := fm.Up()
		if migrationErr != nil {
			f, fileErr := os.OpenFile("error.log", os.O_RDWR|os.O_CREATE|os.O_APPEND, 0666)
			if fileErr != nil {
				// log.Fatalf("error opening file: %v", migrationErr)
			}
			defer f.Close()
			log.SetOutput(f)
			log.Println(migrationErr)
		}
	}

	err = DB.RawQuery("VACUUM").Exec()
	if err != nil {
		fmt.Println(err)
	}

	pop.Debug = env == "dev"
}
