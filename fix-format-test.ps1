Write-Host "==> Running scalafmt..."
sbt scalafmtAll
if ($LASTEXITCODE -ne 0) { Write-Error "scalafmt failed"; exit 1 }

Write-Host "==> Running scalafix..."
sbt scalafixAll
if ($LASTEXITCODE -ne 0) { Write-Error "scalafix failed"; exit 1 }

Write-Host "==> Running tests..."
sbt test
if ($LASTEXITCODE -ne 0) { Write-Error "tests failed"; exit 1 }
