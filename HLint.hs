-- HLint configuration file

-- Related to 'Hide post-AMP warnings' comment
ignore "Use fewer imports" = System.Process.Read

-- Related to 'explicit pattern matching is clearer' comment
ignore "Use fromMaybe" = Stack.Types.Config.explicitSetupDeps
