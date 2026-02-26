package com.eib.breedingdesigner.ui.screens

import androidx.compose.animation.AnimatedVisibility
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.*
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import com.eib.breedingdesigner.model.*
import com.eib.breedingdesigner.ui.components.*
import com.eib.breedingdesigner.viewmodel.ScenarioViewModel

/**
 * Ranges exploration screen – mirrors the "Ranges" tab of the R Shiny app.
 *
 * Left (Setup) section:
 *   • Variances (varG, varGxL, varGxY, plot error)
 *   • Range sliders for first-stage entries / years / locs / reps
 *   • Costs, final varieties, repeats
 *   • "Run Ranges" button
 *
 * Right (Results) section:
 *   • Tab: Heatmap  – X/Y axis selector + fixed-param selectors
 *   • Tab: Lines    – X / treatment axis selector
 */
@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun RangesScreen(vm: ScenarioViewModel) {
    val rs by vm.rangesState.collectAsState()
    val cfg = rs.config

    Column(
        modifier = Modifier
            .fillMaxSize()
            .verticalScroll(rememberScrollState())
            .padding(16.dp),
        verticalArrangement = Arrangement.spacedBy(16.dp)
    ) {
        // ── Error banner ──────────────────────────────────────────────────────
        rs.errorMsg?.let { msg ->
            Card(colors = CardDefaults.cardColors(containerColor = MaterialTheme.colorScheme.errorContainer)) {
                Row(Modifier.padding(12.dp), verticalAlignment = Alignment.CenterVertically) {
                    Text(msg, Modifier.weight(1f), color = MaterialTheme.colorScheme.onErrorContainer)
                    IconButton(onClick = { vm.clearRangesError() }) {
                        Icon(Icons.Default.Close, "Dismiss")
                    }
                }
            }
        }

        // ── Setup card ────────────────────────────────────────────────────────
        RangesSetupCard(cfg, vm)

        // ── Results ───────────────────────────────────────────────────────────
        when {
            rs.isRunning -> Box(Modifier.fillMaxWidth().height(200.dp),
                contentAlignment = Alignment.Center) {
                Column(horizontalAlignment = Alignment.CenterHorizontally,
                    verticalArrangement = Arrangement.spacedBy(12.dp)) {
                    CircularProgressIndicator()
                    Text("Running range simulation…", style = MaterialTheme.typography.bodyMedium)
                }
            }

            rs.result != null -> RangesResults(rs, vm)

            else -> Box(Modifier.fillMaxWidth().height(160.dp),
                contentAlignment = Alignment.Center) {
                Text(
                    "Configure ranges above and press\nRun Ranges to explore parameter sensitivity.",
                    style = MaterialTheme.typography.bodyMedium,
                    textAlign = TextAlign.Center,
                    color = MaterialTheme.colorScheme.onSurfaceVariant
                )
            }
        }
    }
}

// ─── Setup card ──────────────────────────────────────────────────────────────

@Composable
private fun RangesSetupCard(cfg: RangesConfig, vm: ScenarioViewModel) {
    var expanded by remember { mutableStateOf(true) }

    Card(
        modifier = Modifier.fillMaxWidth(),
        elevation = CardDefaults.cardElevation(2.dp)
    ) {
        Column(Modifier.padding(16.dp), verticalArrangement = Arrangement.spacedBy(12.dp)) {
            // Header
            Row(Modifier.fillMaxWidth(), verticalAlignment = Alignment.CenterVertically) {
                Text(
                    "Range Configuration",
                    style = MaterialTheme.typography.titleMedium,
                    color = MaterialTheme.colorScheme.primary,
                    modifier = Modifier.weight(1f)
                )
                IconButton(onClick = { expanded = !expanded }) {
                    Icon(
                        if (expanded) Icons.Default.ExpandLess else Icons.Default.ExpandMore,
                        contentDescription = null
                    )
                }
            }

            AnimatedVisibility(expanded) {
                Column(verticalArrangement = Arrangement.spacedBy(12.dp)) {
                    // Variances
                    Divider()
                    Text("Variances", style = MaterialTheme.typography.labelLarge)
                    Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                        DoubleInput("σ²G", cfg.varG,
                            { vm.updateRangesConfig(cfg.copy(varG = it)) }, Modifier.weight(1f))
                        DoubleInput("σ²GxL", cfg.varGxL,
                            { vm.updateRangesConfig(cfg.copy(varGxL = it)) }, Modifier.weight(1f))
                        DoubleInput("σ²GxY", cfg.varGxY,
                            { vm.updateRangesConfig(cfg.copy(varGxY = it)) }, Modifier.weight(1f))
                    }
                    DoubleInput("Plot Error Variance", cfg.errorVariance,
                        { vm.updateRangesConfig(cfg.copy(errorVariance = it)) },
                        Modifier.fillMaxWidth())

                    // Parameter ranges
                    Divider()
                    Text("First-Stage Parameter Ranges", style = MaterialTheme.typography.labelLarge)

                    RangeParamRow("Entries",
                        cfg.entriesRange,
                        minBound = 10, maxBound = 5000) {
                        vm.updateRangesConfig(cfg.copy(entriesRange = it))
                    }
                    RangeParamRow("Years",
                        cfg.yearsRange,
                        minBound = 1, maxBound = 10) {
                        vm.updateRangesConfig(cfg.copy(yearsRange = it))
                    }
                    RangeParamRow("Locations",
                        cfg.locsRange,
                        minBound = 1, maxBound = 20) {
                        vm.updateRangesConfig(cfg.copy(locsRange = it))
                    }
                    RangeParamRow("Reps/Loc",
                        cfg.repsRange,
                        minBound = 1, maxBound = 30) {
                        vm.updateRangesConfig(cfg.copy(repsRange = it))
                    }

                    // Costs
                    Divider()
                    Text("Costs (first stage)", style = MaterialTheme.typography.labelLarge)
                    Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                        DoubleInput("Plot ($)", cfg.plotCost,
                            { vm.updateRangesConfig(cfg.copy(plotCost = it)) },
                            Modifier.weight(1f), min = 0.0)
                        DoubleInput("Location ($)", cfg.locCost,
                            { vm.updateRangesConfig(cfg.copy(locCost = it)) },
                            Modifier.weight(1f), min = 0.0)
                        DoubleInput("Fixed ($)", cfg.fixedCost,
                            { vm.updateRangesConfig(cfg.copy(fixedCost = it)) },
                            Modifier.weight(1f), min = 0.0)
                    }

                    // Selection
                    Divider()
                    Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                        IntInput("Mult. Years", cfg.multYears,
                            { vm.updateRangesConfig(cfg.copy(multYears = it)) },
                            Modifier.weight(1f), min = 0)
                        IntInput("Final Varieties", cfg.varieties,
                            { vm.updateRangesConfig(cfg.copy(varieties = it)) },
                            Modifier.weight(1f), min = 1)
                        IntInput("Replicates", cfg.nRepeats,
                            { vm.updateRangesConfig(cfg.copy(nRepeats = it)) },
                            Modifier.weight(1f), min = 10, max = 1000)
                    }
                }
            }

            // Run button
            Button(
                onClick = { vm.runRanges() },
                modifier = Modifier.fillMaxWidth()
            ) {
                Icon(Icons.Default.PlayArrow, null)
                Spacer(Modifier.width(8.dp))
                Text("Run Ranges")
            }
        }
    }
}

// ─── Range param row (min / max / samples) ───────────────────────────────────

@Composable
private fun RangeParamRow(
    label:    String,
    param:    RangeParam,
    minBound: Int,
    maxBound: Int,
    onChange: (RangeParam) -> Unit
) {
    Column {
        Text(label, style = MaterialTheme.typography.bodySmall,
            color = MaterialTheme.colorScheme.secondary)
        Row(horizontalArrangement = Arrangement.spacedBy(6.dp)) {
            IntInput("Min", param.min,
                { onChange(param.copy(min = it.coerceAtMost(param.max))) },
                Modifier.weight(1f), min = minBound, max = maxBound)
            IntInput("Max", param.max,
                { onChange(param.copy(max = it.coerceAtLeast(param.min))) },
                Modifier.weight(1f), min = minBound, max = maxBound)
            IntInput("Steps", param.samples,
                { onChange(param.copy(samples = it)) },
                Modifier.weight(0.7f), min = 2, max = 7)
        }
    }
}

// ─── Results section ─────────────────────────────────────────────────────────

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun RangesResults(
    rs: com.eib.breedingdesigner.model.RangesState,
    vm: ScenarioViewModel
) {
    val result = rs.result ?: return
    var tab by remember { mutableStateOf(0) }

    Card(Modifier.fillMaxWidth(), elevation = CardDefaults.cardElevation(2.dp)) {
        Column(Modifier.padding(16.dp), verticalArrangement = Arrangement.spacedBy(12.dp)) {
            Text("Results", style = MaterialTheme.typography.titleMedium,
                color = MaterialTheme.colorScheme.primary)

            // Summary chip
            val nPoints = result.points.size
            val maxGain = result.points.maxOfOrNull { it.meanGain } ?: 0.0
            val minGain = result.points.minOfOrNull { it.meanGain } ?: 0.0
            Text(
                "$nPoints combinations | Gain range: ${"%.3f".format(minGain)} – ${"%.3f".format(maxGain)}",
                style = MaterialTheme.typography.bodySmall,
                color = MaterialTheme.colorScheme.secondary
            )

            // Tabs
            TabRow(selectedTabIndex = tab) {
                Tab(selected = tab == 0, onClick = { tab = 0 },
                    text = { Text("Heatmap") },
                    icon = { Icon(Icons.Default.GridOn, null) })
                Tab(selected = tab == 1, onClick = { tab = 1 },
                    text = { Text("Sensitivity") },
                    icon = { Icon(Icons.Default.ShowChart, null) })
            }

            when (tab) {
                0 -> HeatmapTab(result, rs, vm)
                1 -> SensitivityTab(result, rs, vm)
            }
        }
    }
}

@Composable
private fun HeatmapTab(
    result: RangeResult,
    rs:     com.eib.breedingdesigner.model.RangesState,
    vm:     ScenarioViewModel
) {
    Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
        // Axis selectors
        AxisSelector("X axis", rs.xAxis,
            exclude = rs.yAxis,
            onSelect = { vm.updateRangesXAxis(it) })
        AxisSelector("Y axis", rs.yAxis,
            exclude = rs.xAxis,
            onSelect = { vm.updateRangesYAxis(it) })

        // Fixed-value selectors for remaining axes
        val remaining = RangeAxis.entries.filter { it != rs.xAxis && it != rs.yAxis }
        remaining.forEach { ax ->
            val vals   = result.axisValues(ax)
            val current = rs.fixedValues[ax] ?: vals.first()
            FixedAxisSelector(ax.label, current, vals) { vm.updateRangesFixed(ax, it) }
        }

        Spacer(Modifier.height(4.dp))

        HeatmapChart(
            result   = result,
            xAxis    = rs.xAxis,
            yAxis    = rs.yAxis,
            fixed    = rs.fixedValues,
            modifier = Modifier.fillMaxWidth()
        )
    }
}

@Composable
private fun SensitivityTab(
    result: RangeResult,
    rs:     com.eib.breedingdesigner.model.RangesState,
    vm:     ScenarioViewModel
) {
    Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
        AxisSelector("X axis", rs.xAxisLine,
            exclude = rs.treatmentAxis,
            onSelect = { vm.updateRangesXLine(it) })
        AxisSelector("Treatment", rs.treatmentAxis,
            exclude = rs.xAxisLine,
            onSelect = { vm.updateRangesTreatment(it) })

        val remaining = RangeAxis.entries.filter { it != rs.xAxisLine && it != rs.treatmentAxis }
        remaining.forEach { ax ->
            val vals   = result.axisValues(ax)
            val current = rs.fixedValues[ax] ?: vals.first()
            FixedAxisSelector(ax.label, current, vals) { vm.updateRangesFixed(ax, it) }
        }

        Spacer(Modifier.height(4.dp))

        SensitivityLineChart(
            result        = result,
            xAxis         = rs.xAxisLine,
            treatmentAxis = rs.treatmentAxis,
            fixed         = rs.fixedValues,
            modifier      = Modifier.fillMaxWidth()
        )
    }
}

// ─── Small selectors ─────────────────────────────────────────────────────────

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun AxisSelector(
    label:    String,
    current:  RangeAxis,
    exclude:  RangeAxis,
    onSelect: (RangeAxis) -> Unit
) {
    val choices = RangeAxis.entries.filter { it != exclude }
    var expanded by remember { mutableStateOf(false) }
    ExposedDropdownMenuBox(expanded = expanded, onExpandedChange = { expanded = it }) {
        OutlinedTextField(
            value = current.label,
            onValueChange = {},
            label = { Text(label) },
            readOnly = true,
            trailingIcon = { ExposedDropdownMenuDefaults.TrailingIcon(expanded) },
            modifier = Modifier.fillMaxWidth().menuAnchor()
        )
        ExposedDropdownMenu(expanded = expanded, onDismissRequest = { expanded = false }) {
            choices.forEach { ax ->
                DropdownMenuItem(
                    text = { Text(ax.label) },
                    onClick = { onSelect(ax); expanded = false }
                )
            }
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun FixedAxisSelector(
    label:    String,
    current:  Int,
    values:   List<Int>,
    onSelect: (Int) -> Unit
) {
    var expanded by remember { mutableStateOf(false) }
    ExposedDropdownMenuBox(expanded = expanded, onExpandedChange = { expanded = it }) {
        OutlinedTextField(
            value = current.toString(),
            onValueChange = {},
            label = { Text("Fix: $label") },
            readOnly = true,
            trailingIcon = { ExposedDropdownMenuDefaults.TrailingIcon(expanded) },
            modifier = Modifier.fillMaxWidth().menuAnchor()
        )
        ExposedDropdownMenu(expanded = expanded, onDismissRequest = { expanded = false }) {
            values.forEach { v ->
                DropdownMenuItem(
                    text = { Text("$v") },
                    onClick = { onSelect(v); expanded = false }
                )
            }
        }
    }
}
