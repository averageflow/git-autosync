# git-autosync

A humble effort to create a helpful tool that will keep git repositories up to date with your remotes, in a very customizable fashion, with YAML config.
The concept is to sync from local machine to remote by committing and pushing (optionally pulling first WIP). This could then easily be added to a cron job for automation.
The trigger for syncing is the existence of uncommitted changes.

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
      defaultCommitMessage: "Programmatic commit from Haskell code, so this project commits and pushes itself"
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
      defaultCommitMessage: "Programmatic commit from Haskell code, updating my Knowledge Base"
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

To install this to your Unix system, build the tool with `stack build --test --copy-bins`. Take note of the mentioned path.
For me the binary was copied to `/home/joe/.local/bin/git-autosync-exe`. I made sure that this folder is in my `PATH` and now I can call the tool from anywhere.

Next I simply create a `.gitautosync.yaml` in my home folder, and I can use `git-autosync-exe` command from home folder now.
If running in a cron, make sure to change your working directory to where the `.gitautosync.yaml` file is located.

