$ErrorActionPreference = "Stop"

$Root = Split-Path -Parent $MyInvocation.MyCommand.Path
$Rscript = "C:\Program Files\R\R-4.6.1\bin\Rscript.exe"
$PackageRepo = "C:\Users\cattaneo\Dropbox\software\mdcattaneo\ramchoice"
$SimulationScript = Join-Path $Root "CCMM_2026_wp--simuls.R"
$OutputDir = Join-Path $Root "output"
$Timestamp = Get-Date -Format "yyyyMMdd-HHmmss"
$LogDir = Join-Path $OutputDir "production-logs\$Timestamp"

if (-not (Test-Path -LiteralPath $Rscript)) {
    throw "Rscript was not found at $Rscript"
}
New-Item -ItemType Directory -Force -Path $LogDir | Out-Null

$env:RAMCHOICE_GIT_SHA = (& git -C $PackageRepo rev-parse HEAD).Trim()
$env:REPLICATION_GIT_SHA = (& git -C $Root rev-parse HEAD).Trim()
if ($LASTEXITCODE -ne 0) {
    throw "Could not determine the replication Git commit."
}

$blocks = @("homogeneous-aom", "hlao", "hlao-diagnostic")
$jobs = foreach ($block in $blocks) {
    $stdout = Join-Path $LogDir "$block.out.log"
    $stderr = Join-Path $LogDir "$block.err.log"
    $process = Start-Process `
        -FilePath $Rscript `
        -ArgumentList @($SimulationScript, "--block=$block") `
        -WorkingDirectory $Root `
        -WindowStyle Hidden `
        -RedirectStandardOutput $stdout `
        -RedirectStandardError $stderr `
        -PassThru
    [pscustomobject]@{
        Block = $block
        Process = $process
        Stdout = $stdout
        Stderr = $stderr
    }
}

$manifest = [pscustomobject]@{
    StartedAt = (Get-Date).ToString("o")
    RamchoiceGitSha = $env:RAMCHOICE_GIT_SHA
    ReplicationGitSha = $env:REPLICATION_GIT_SHA
    Rscript = $Rscript
    Jobs = @($jobs | ForEach-Object {
        [pscustomobject]@{
            Block = $_.Block
            ProcessId = $_.Process.Id
            Stdout = $_.Stdout
            Stderr = $_.Stderr
        }
    })
}
$manifest | ConvertTo-Json -Depth 4 | Set-Content `
    -LiteralPath (Join-Path $LogDir "manifest.json") `
    -Encoding UTF8

Write-Host "Production simulations started."
Write-Host "Logs: $LogDir"
$jobs | ForEach-Object {
    Write-Host ("  {0}: PID {1}" -f $_.Block, $_.Process.Id)
}
Write-Host "This window will remain open while the jobs run."

$failures = @()
foreach ($job in $jobs) {
    $job.Process.WaitForExit()
    if ($job.Process.ExitCode -ne 0) {
        $failures += $job
    }
    Write-Host ("Completed {0} with exit code {1}." -f `
        $job.Block, $job.Process.ExitCode)
}

if ($failures.Count -gt 0) {
    $failedBlocks = ($failures | ForEach-Object { $_.Block }) -join ", "
    throw "Production simulation failed for: $failedBlocks. Check $LogDir"
}

& $Rscript (Join-Path $Root "CCMM_2026_wp--tables.R")
if ($LASTEXITCODE -ne 0) {
    throw "Production table rendering failed."
}
& $Rscript (Join-Path $Root "CCMM_2026_wp--figures.R")
if ($LASTEXITCODE -ne 0) {
    throw "Production figure rendering failed."
}

[pscustomobject]@{
    CompletedAt = (Get-Date).ToString("o")
    Status = "complete"
    LogDirectory = $LogDir
} | ConvertTo-Json | Set-Content `
    -LiteralPath (Join-Path $LogDir "status.json") `
    -Encoding UTF8

Write-Host "Production simulations and rendering completed successfully."
