# git-autosync

A humble effort to create a helpful tool that will keep git repositories up to date with your remotes, in a very customizable fashion, with YAML config.
The concept is to sync from local machine to remote by committing and pushing (optionally pulling first). This can be added to a cron job for automation.
The trigger for syncing can be either the existence of uncommitted changes, or (not yet implemented) the diff between current and wanted branch.


You can run this project with:
```
stack build && stack exec git-autosync-exe
```