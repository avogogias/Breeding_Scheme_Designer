package com.eib.breedingdesigner.ui.components

import androidx.compose.animation.AnimatedVisibility
import androidx.compose.foundation.layout.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.ExpandLess
import androidx.compose.material.icons.filled.ExpandMore
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import com.eib.breedingdesigner.model.Scenario
import com.eib.breedingdesigner.model.Stage

@Composable
fun StageEditorList(
    scenario: Scenario,
    onStageChanged: (Int, Stage) -> Unit,
    onAddStage: () -> Unit,
    onRemoveStage: () -> Unit,
    modifier: Modifier = Modifier
) {
    Column(modifier) {
        scenario.stages.forEachIndexed { idx, stage ->
            StageCard(
                index = idx,
                stage = stage,
                varG = scenario.varG, varGxL = scenario.varGxL, varGxY = scenario.varGxY,
                onChange = { onStageChanged(idx, it) }
            )
            Spacer(Modifier.height(8.dp))
        }
        Row(
            horizontalArrangement = Arrangement.spacedBy(8.dp),
            modifier = Modifier.fillMaxWidth()
        ) {
            OutlinedButton(
                onClick = onAddStage,
                modifier = Modifier.weight(1f)
            ) { Text("+ Add Stage") }
            OutlinedButton(
                onClick = onRemoveStage,
                enabled = scenario.stages.size > 1,
                modifier = Modifier.weight(1f)
            ) { Text("− Remove Stage") }
        }
    }
}

@Composable
private fun StageCard(
    index: Int,
    stage: Stage,
    varG: Double, varGxL: Double, varGxY: Double,
    onChange: (Stage) -> Unit
) {
    var expanded by remember { mutableStateOf(index == 0) }
    val h2 = stage.heritability(varG, varGxL, varGxY)

    Card(
        colors = CardDefaults.cardColors(containerColor = MaterialTheme.colorScheme.surfaceVariant)
    ) {
        Column(Modifier.padding(12.dp)) {
            // Header row
            Row(
                verticalAlignment = Alignment.CenterVertically,
                modifier = Modifier.fillMaxWidth()
            ) {
                Text(
                    "Stage ${index + 1}",
                    style = MaterialTheme.typography.titleSmall.copy(fontWeight = FontWeight.Bold),
                    modifier = Modifier.weight(1f)
                )
                Text(
                    "h² = ${"%.2f".format(h2)}",
                    style = MaterialTheme.typography.bodySmall,
                    color = MaterialTheme.colorScheme.secondary
                )
                Spacer(Modifier.width(8.dp))
                IconButton(onClick = { expanded = !expanded }) {
                    Icon(
                        if (expanded) Icons.Default.ExpandLess else Icons.Default.ExpandMore,
                        contentDescription = if (expanded) "Collapse" else "Expand"
                    )
                }
            }

            // Summary line when collapsed
            if (!expanded) {
                Text(
                    "Entries: ${stage.entries}  Years: ${stage.years}  Locs: ${stage.locs}  Reps: ${stage.reps}",
                    style = MaterialTheme.typography.bodySmall,
                    color = MaterialTheme.colorScheme.onSurfaceVariant
                )
            }

            AnimatedVisibility(visible = expanded) {
                Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
                    Spacer(Modifier.height(4.dp))
                    Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                        IntInput(
                            "Entries", stage.entries,
                            { onChange(stage.copy(entries = it)) },
                            Modifier.weight(1f), min = 1
                        )
                        IntInput(
                            "Years", stage.years,
                            { onChange(stage.copy(years = it)) },
                            Modifier.weight(1f), min = 1
                        )
                    }
                    Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                        IntInput(
                            "Locations", stage.locs,
                            { onChange(stage.copy(locs = it)) },
                            Modifier.weight(1f), min = 1
                        )
                        IntInput(
                            "Reps/Loc", stage.reps,
                            { onChange(stage.copy(reps = it)) },
                            Modifier.weight(1f), min = 1
                        )
                    }
                    DoubleInput(
                        "Plot Error Variance", stage.errorVariance,
                        { onChange(stage.copy(errorVariance = it)) },
                        Modifier.fillMaxWidth(), min = 0.001
                    )
                    Divider()
                    Text("Costs", style = MaterialTheme.typography.labelMedium)
                    Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                        DoubleInput(
                            "Plot ($)", stage.plotCost,
                            { onChange(stage.copy(plotCost = it)) },
                            Modifier.weight(1f), min = 0.0
                        )
                        DoubleInput(
                            "Location ($)", stage.locCost,
                            { onChange(stage.copy(locCost = it)) },
                            Modifier.weight(1f), min = 0.0
                        )
                    }
                    DoubleInput(
                        "Fixed Cost ($)", stage.fixedCost,
                        { onChange(stage.copy(fixedCost = it)) },
                        Modifier.fillMaxWidth(), min = 0.0
                    )
                    Text(
                        "Stage cost: $${"%.0f".format(stage.cost())}",
                        style = MaterialTheme.typography.bodySmall,
                        color = MaterialTheme.colorScheme.secondary
                    )
                }
            }
        }
    }
}
