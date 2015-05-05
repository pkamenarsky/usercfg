# usercfg

usercfg is a remote account configuration tool for developers.

Given a server implementing the open protocol, usercfg offers easy and passwordless command-line management of multiple accounts on multiple sites.

The reference implementation is based on a flexible protocol proposal allowing a conforming server to offer a wide variety of account managament commands. Those are then presented to the user by a client-side command line tool.

Currently there's a reference server in Haskell and a client in Python. However, the protocol is simple enough to allow for and hopefully encourage alternative implementations.

The Haskell server implementation offers password and passwordless pubkey authentication and SendGrid intergration for password reset requests. It is based on the [users](https://github.com/agrafix/users) library, which supports interchangeable storage backends and comes with a PostgreSQL implementation. Passwords are hashed using bcrypt. It is easily deployable to Heroku without any Haskell knowledge.

# Client installation

If you trust me

    curl https://raw.githubusercontent.com/pkamenarsky/usercfg/master/client/install.sh | /bin/bash

Otherwise, just do what the script does

    pip install --user paramiko
    sudo curl https://raw.githubusercontent.com/pkamenarsky/usercfg/master/client/usercfg > /usr/local/bin/usercfg
    sudo chmod +x /usr/local/bin/usercfg

## Examples

    usercfg example.com create-user --name NAME --email EMAIL --ssh-key

creates a user on the site `example.com`. Automatically uploads the user's public ssh key. A shorter version would be:

    usercfg example.com create-user -n NAME -e EMAIL -S

To update a user's email:

    usercfg example.com update-user -n NAME -e NEW-EMAIL

To request a password reset token:

    usercfg example.com reset-password -n NAME

To enter a new password after receiving a mail with the password reset token:

    usercfg example.com apply-password -n NAME -t TOKEN


# Server installation

Install the [Haskell platform](https://www.haskell.org/platform). Then

    git clone https://github.com/pkamenarsky/usercfg
    cd usercfg
    cabal install

Go grab a beer and when you're back and all went well, you'll have a `usercfg-server` binary in `~/.cabal/bin`.

# Configuration

Install PostgreSQL. Set the following environment variables

* `$PORT` - endpoint port (optional, default: 8000)
* `$DATABASE_URL` - database connection string, in the PostgreSQL [format](http://www.postgresql.org/docs/9.4/static/libpq-connect.html#AEN41094) (optional, default: localhost)

If you want to use SendGrid for password reset requests, you'll also need to set the following variables

* `SG_USER` - SendGrid username
* `SG_PASS` - SendGrid password
* `SG_FROM` - From field
* `SG_SUBJ` - Subject line
* `SG_TEXT` - Email text, use `$TOKEN` as a placeholder for the password reset token

After setting all needed variables, start PostgresSQL and then launch `~/.cabal/bin/usercfg-server`

# Deploy to Heroku

In the repository

    heroku create --stack=cedar-14 --buildpack https://github.com/begriffs/heroku-buildpack-ghc.git
    git push heroku master

After deploying you'll need to provision a PostgreSQL database for the deployed app. Then just do

    heroku restart

# Future ideas

* User roles
* OAuth2 support
* 2-factor authentication
* Password recovery per text message
