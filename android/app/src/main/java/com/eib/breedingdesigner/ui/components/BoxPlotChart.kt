package com.eib.breedingdesigner.ui.components

import androidx.compose.foundation.Canvas
import androidx.compose.foundation.layout.*
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.drawscope.DrawScope
import androidx.compose.ui.graphics.drawscope.Stroke
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import com.eib.breedingdesigner.model.ScenarioResult
import com.eib.breedingdesigner.model.StageStats
import kotlin.math.abs

/**
 * Draws a set of box-and-whisker plots for a single scenario result,
 * one box per breeding stage.
 */
@Composable
fun BoxPlotChart(
    result: ScenarioResult,
    modifier: Modifier = Modifier
) {
    val stages = result.result.stageStats
    if (stages.isEmpty()) return

    val primaryColor = MaterialTheme.colorScheme.primary
    val secondaryColor = MaterialTheme.colorScheme.secondary
    val onSurface = MaterialTheme.colorScheme.onSurface

    Column(modifier) {
        Text(
            "Genetic Gain by Stage",
            style = MaterialTheme.typography.titleSmall,
            modifier = Modifier.padding(bottom = 4.dp)
        )

        Canvas(
            modifier = Modifier
                .fillMaxWidth()
                .height(220.dp)
        ) {
            drawBoxPlots(stages, primaryColor, secondaryColor, onSurface)
        }

        // X-axis labels
        Row(
            modifier = Modifier.fillMaxWidth().padding(horizontal = 8.dp),
            horizontalArrangement = Arrangement.SpaceAround
        ) {
            stages.forEachIndexed { i, _ ->
                Text(
                    "S${i + 1}",
                    style = MaterialTheme.typography.labelSmall,
                    textAlign = TextAlign.Center,
                    modifier = Modifier.weight(1f)
                )
            }
        }
    }
}

private fun DrawScope.drawBoxPlots(
    stages: List<StageStats>,
    boxColor: Color,
    medianColor: Color,
    axisColor: Color
) {
    val pad = 40f      // left padding for y-axis labels
    val padRight = 8f
    val padTop = 8f
    val padBottom = 24f

    val chartWidth = size.width - pad - padRight
    val chartHeight = size.height - padTop - padBottom
    val n = stages.size
    val slotW = chartWidth / n

    // Determine y range
    val allVals = stages.flatMap { listOf(it.min, it.max) }
    val yMin = allVals.minOrNull() ?: 0.0
    val yMax = allVals.maxOrNull() ?: 1.0
    val yRange = if (abs(yMax - yMin) < 1e-9) 1.0 else yMax - yMin

    fun yPx(v: Double): Float =
        padTop + chartHeight - ((v - yMin) / yRange * chartHeight).toFloat()

    // Y-axis grid lines (5 lines)
    val stroke2 = Stroke(width = 1f)
    val gridColor = axisColor.copy(alpha = 0.15f)
    for (k in 0..4) {
        val yVal = yMin + k * yRange / 4
        val yp = yPx(yVal)
        drawLine(gridColor, Offset(pad, yp), Offset(size.width - padRight, yp), strokeWidth = 1f)
    }

    // Boxes
    stages.forEachIndexed { i, st ->
        val cx = pad + slotW * i + slotW / 2
        val boxHalfW = slotW * 0.28f

        val q3px = yPx(st.q75)
        val q1px = yPx(st.q25)
        val medPx = yPx(st.median)
        val minPx = yPx(st.min)
        val maxPx = yPx(st.max)

        // Whisker lines
        drawLine(boxColor, Offset(cx, minPx), Offset(cx, q1px), strokeWidth = 2f)
        drawLine(boxColor, Offset(cx, q3px), Offset(cx, maxPx), strokeWidth = 2f)
        // Whisker caps
        drawLine(boxColor, Offset(cx - boxHalfW * 0.5f, minPx), Offset(cx + boxHalfW * 0.5f, minPx), strokeWidth = 2f)
        drawLine(boxColor, Offset(cx - boxHalfW * 0.5f, maxPx), Offset(cx + boxHalfW * 0.5f, maxPx), strokeWidth = 2f)

        // Box fill
        drawRect(
            color = boxColor.copy(alpha = 0.18f),
            topLeft = Offset(cx - boxHalfW, q3px),
            size = Size(boxHalfW * 2, q1px - q3px)
        )
        // Box outline
        drawRect(
            color = boxColor,
            topLeft = Offset(cx - boxHalfW, q3px),
            size = Size(boxHalfW * 2, q1px - q3px),
            style = Stroke(width = 2.5f)
        )
        // Median line
        drawLine(
            medianColor,
            Offset(cx - boxHalfW, medPx),
            Offset(cx + boxHalfW, medPx),
            strokeWidth = 3f
        )
    }

    // Y-axis line
    drawLine(axisColor.copy(alpha = 0.5f), Offset(pad, padTop), Offset(pad, size.height - padBottom), strokeWidth = 1.5f)
}

/**
 * Multi-scenario comparison chart – one coloured median line per scenario.
 */
@Composable
fun CompareChart(
    results: List<ScenarioResult>,
    modifier: Modifier = Modifier
) {
    if (results.isEmpty()) return
    val maxStages = results.maxOf { it.result.stageStats.size }
    val colors = listOf(
        Color(0xFF2E7D32), Color(0xFF1565C0), Color(0xFFE65100),
        Color(0xFF6A1B9A), Color(0xFF00838F)
    )

    Column(modifier) {
        Text(
            "Scenario Comparison (median genetic gain)",
            style = MaterialTheme.typography.titleSmall,
            modifier = Modifier.padding(bottom = 4.dp)
        )

        // Legend
        Row(horizontalArrangement = Arrangement.spacedBy(12.dp), modifier = Modifier.padding(bottom = 4.dp)) {
            results.forEachIndexed { i, sr ->
                Row(verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(4.dp)) {
                    Canvas(Modifier.size(12.dp)) {
                        drawCircle(colors[i % colors.size])
                    }
                    Text(sr.scenario.name, style = MaterialTheme.typography.labelSmall)
                }
            }
        }

        val primaryColor = MaterialTheme.colorScheme.onSurface
        Canvas(
            modifier = Modifier
                .fillMaxWidth()
                .height(180.dp)
        ) {
            val pad = 36f; val padR = 8f; val padT = 8f; val padB = 20f
            val cW = size.width - pad - padR
            val cH = size.height - padT - padB

            val allMeans = results.flatMap { it.result.stageStats.map { s -> s.mean } }
            val yMin = allMeans.minOrNull() ?: 0.0
            val yMax = allMeans.maxOrNull() ?: 1.0
            val yRange = if (abs(yMax - yMin) < 1e-9) 1.0 else yMax - yMin
            fun yPx(v: Double) = padT + cH - ((v - yMin) / yRange * cH).toFloat()
            fun xPx(s: Int, total: Int) = pad + (s.toFloat() / (total - 1).coerceAtLeast(1)) * cW

            // Grid
            for (k in 0..4) {
                val yp = yPx(yMin + k * yRange / 4)
                drawLine(primaryColor.copy(alpha = 0.12f), Offset(pad, yp), Offset(size.width - padR, yp), 1f)
            }

            results.forEachIndexed { i, sr ->
                val col = colors[i % colors.size]
                val pts = sr.result.stageStats.mapIndexed { s, st ->
                    Offset(xPx(s, maxStages), yPx(st.median))
                }
                for (j in 1 until pts.size) {
                    drawLine(col, pts[j - 1], pts[j], strokeWidth = 3f)
                }
                pts.forEach { pt -> drawCircle(col, radius = 6f, center = pt) }
            }

            drawLine(primaryColor.copy(alpha = 0.5f), Offset(pad, padT), Offset(pad, size.height - padB), 1.5f)
        }

        Row(
            modifier = Modifier.fillMaxWidth().padding(horizontal = 4.dp),
            horizontalArrangement = Arrangement.SpaceAround
        ) {
            (1..maxStages).forEach { s ->
                Text("S$s", style = MaterialTheme.typography.labelSmall, textAlign = TextAlign.Center, modifier = Modifier.weight(1f))
            }
        }
    }
}
