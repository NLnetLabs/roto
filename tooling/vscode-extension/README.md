# Roto VS Code Extension

To test this, first package it (with `yes` because it asks annoying questions):

```
yes | npx @vscode/vsce package
```

Then install:

```
code --install-extension roto-0.0.1.vsix
```

And finally run `code` and repeat for every change you make.
