# git-autosync

A humble effort to create a helpful tool that will keep git repositories up to date with your remotes, in a very customizable fashion, with YAML config.
The concept is to sync from local machine to remote by committing and pushing (optionally pulling first WIP). This could then easily be added to a cron job for automation.
The trigger for syncing is the existence of uncommitted changes.

This project has one purpose: to be a tool that can be installed to a Unix system, configurable with YAML, that can add, commit and push from several local repositories to remotes.

For example I have a knowledge base of markdown documents that I like to have synced to a private GitHub repo, and for that I had a little shell script doing it.

Then the idea arose in my head to create something more powerful and configurable that could do this for multiple repos. Of course I love Haskell and want to learn more, and I thought this might be a good way to roll up my sleeves and absorb a lot of knowledge in practice.

You can build and run this project with:
```
stack build && stack exec git-autosync-exe
```

This project expects a config file named `.gitautosync.yaml` to be present in the directory where the tool is installed, or if running from source, in the root directory of the project. This file should contain something like:

```yaml
managedObjects:
  - location: /home/joe/Development/Personal/git-autosync
    commitPreferences:
      includeDateInCommitMessage: true
      defaultCommitMessage: "λ Commit from Haskell code, so this project commits and pushes itself"
      argsForCommitAction: []
    pushPreferences:
      pushToRemoteAfterCommit: true
      argsForPushAction: []
    addPreferences:
      addAllBeforeCommitting: true
      argsForAddAction: []

  - location: /home/joe/Documents/KnowledgeBase
    commitPreferences:
      includeDateInCommitMessage: false
      defaultCommitMessage: "λ Commit from Haskell code, updating my Knowledge Base"
      argsForCommitAction: []
    pushPreferences:
      pushToRemoteAfterCommit: true
      argsForPushAction: []
    addPreferences:
      addAllBeforeCommitting: true
      argsForAddAction: []
```

Note: absolute paths are highly recommended!

## Installing to a Unix system

To install this to your Unix system, build the tool with `cabal build`. You should have a new binary at `dist-newstyle/build/<your os, for me x86_64-linux>/ghc-8.10.7/git-autosync-0.1.0.0/x/git-autosync/build/git-autosync/git-autosync`.

For me I copied the binary to `/home/joe/.local/bin/git-autosync-exe`. I made sure that this folder is in my `PATH` and now I can call the tool from anywhere.

Next I simply create a `.gitautosync.yaml` in my home folder, and I can use `git-autosync-exe` command from home folder now.
If running in a cron, make sure to change your working directory to where the `.gitautosync.yaml` file is located.

I created a crontab for my user with `crontab -e` so that every hour at minute 1 the tool will run and sync my repos to GitHub: 

```sh
SHELL=/usr/bin/zsh
PATH=/home/joe/.local/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/home/joe/bin
MAILTO=joe
HOME=/home/joe

1 * * * * /home/joe/.local/bin/git-autosync-exe
```
