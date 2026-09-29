Write-Host "==> Running scalafmt..."
scala-cli fmt .
if ($LASTEXITCODE -ne 0) { Write-Error "scalafmt failed"; exit 1 }

Write-Host "==> Running scalafix..."
scala-cli fix .
if ($LASTEXITCODE -ne 0) { Write-Error "scalafix failed"; exit 1 }

Write-Host "==> Running tests..."
scala-cli test .
if ($LASTEXITCODE -ne 0) { Write-Error "tests failed"; exit 1 }

Write-Host "==> All steps completed successfully."
