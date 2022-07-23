package models

import (
	"embed"
	"fmt"
	"strings"

	. "github.com/gobuffalo/pop/v6"
)

// NewFileMigrator for a path and a Connection
func EmbeddedNewFileMigrator(c *Connection, migrations embed.FS) (FileMigrator, error) {

	fm := FileMigrator{
		Migrator: NewMigrator(c),
		Path:     "migrations",
	}
	fm.SchemaPath = ""

	runner := func(mf Migration, tx *Connection) error {
		f, err := migrations.Open(mf.Path)
		if err != nil {
			return err
		}
		defer f.Close()
		content, err := MigrationContent(mf, tx, f, true)
		if err != nil {
			return fmt.Errorf("error processing %s: %w", mf.Path, err)
		}
		if content == "" {
			return nil
		}
		err = tx.RawQuery(content).Exec()
		if err != nil {
			return fmt.Errorf("error executing %s, sql: %s: %w", mf.Path, content, err)
		}
		return nil
	}

	err := findMigrations(&fm, migrations, runner)
	if err != nil {
		return fm, err
	}

	return fm, nil
}

func findMigrations(fm *FileMigrator, migrations embed.FS, runner func(mf Migration, tx *Connection) error) error {
	// dir := fm.Path

	fmt.Println("---")
	dir, err := migrations.ReadDir("migrations")
	if err != nil {
		return err
	}

	for _, info := range dir {

		if !info.IsDir() {
			match, err := ParseMigrationFilename(info.Name())
			if err != nil {
				if strings.HasPrefix(err.Error(), "unsupported dialect") {
					// log(logging.Warn, "ignoring migration file with %s", err.Error())
					return nil
				}
				return err
			}
			if match == nil {
				// pop.log(logging.Warn, "ignoring file %s because it does not match the migration file pattern", info.Name())
				return nil
			}
			mf := Migration{
				Path:      "migrations/" + info.Name(),
				Version:   match.Version,
				Name:      match.Name,
				DBType:    match.DBType,
				Direction: match.Direction,
				Type:      match.Type,
				Runner:    runner,
			}
			switch mf.Direction {
			case "up":
				fm.UpMigrations.Migrations = append(fm.UpMigrations.Migrations, mf)
			case "down":
				fm.DownMigrations.Migrations = append(fm.DownMigrations.Migrations, mf)
			default:
				// the regex only matches `(up|down)` for direction, so a panic here is appropriate
				panic("got unknown migration direction " + mf.Direction)
			}
		}
	}
	return nil
}
