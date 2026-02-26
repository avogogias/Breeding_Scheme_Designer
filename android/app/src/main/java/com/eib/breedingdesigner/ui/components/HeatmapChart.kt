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
import com.eib.breedingdesigner.model.*
import kotlin.math.abs

/**
 * 2-D heatmap: colour encodes genetic gain for each (xAxis, yAxis) pair.
 * Green = high gain, amber = medium, red = low.
 */
@Composable
fun HeatmapChart(
    result:   RangeResult,
    xAxis:    RangeAxis,
    yAxis:    RangeAxis,
    fixed:    Map<RangeAxis, Int>,
    modifier: Modifier = Modifier
) {
    val data = result.heatmapData(xAxis, yAxis, fixed)
    if (data.isEmpty()) {
        Text("No data – adjust parameter ranges.", modifier = modifier)
        return
    }

    val xVals = result.axisValues(xAxis)
    val yVals = result.axisValues(yAxis)
    val gains = data.values.map { it.meanGain }
    val minG  = gains.minOrNull() ?: 0.0
    val maxG  = gains.maxOrNull() ?: 1.0
    val range = if (abs(maxG - minG) < 1e-9) 1.0 else maxG - minG

    val onSurface = MaterialTheme.colorScheme.onSurface

    Column(modifier) {
        Text(
            "Genetic Gain – ${xAxis.label} × ${yAxis.label}",
            style = MaterialTheme.typography.titleSmall
        )
        Spacer(Modifier.height(4.dp))

        // Heatmap: Y-axis labels on the left, canvas grid on the right
        Row(Modifier.fillMaxWidth().height(200.dp)) {
            // Y-axis labels column (low→high, bottom→top, so reversed)
            Column(
                modifier = Modifier.width(28.dp).fillMaxHeight(),
                verticalArrangement = Arrangement.SpaceAround
            ) {
                yVals.reversed().forEach { v ->
                    Text(
                        "$v",
                        fontSize = 9.sp,
                        textAlign = TextAlign.End,
                        modifier = Modifier.fillMaxWidth()
                    )
                }
            }

            Canvas(modifier = Modifier.weight(1f).fillMaxHeight()) {
                drawHeatmap(data, xVals, yVals, minG, range, onSurface)
            }
        }

        // X-axis labels
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .padding(start = 28.dp, end = 0.dp),
            horizontalArrangement = Arrangement.SpaceAround
        ) {
            xVals.forEach { v ->
                Text(
                    "$v",
                    style = MaterialTheme.typography.labelSmall,
                    textAlign = TextAlign.Center,
                    modifier = Modifier.weight(1f)
                )
            }
        }
        Text(
            xAxis.label,
            style = MaterialTheme.typography.labelSmall,
            textAlign = TextAlign.Center,
            modifier = Modifier.fillMaxWidth()
        )
    }
}

private fun DrawScope.drawHeatmap(
    data:  Map<Pair<Int, Int>, RangePoint>,
    xVals: List<Int>,
    yVals: List<Int>,
    minG:  Double,
    range: Double,
    axis:  Color
) {
    if (xVals.isEmpty() || yVals.isEmpty()) return
    val w = size.width  / xVals.size
    val h = size.height / yVals.size

    yVals.forEachIndexed { yi, yVal ->
        xVals.forEachIndexed { xi, xVal ->
            val pt   = data[Pair(xVal, yVal)] ?: return@forEachIndexed
            val t    = ((pt.meanGain - minG) / range).toFloat().coerceIn(0f, 1f)
            val fill = gainColor(t)

            val left = xi * w
            val top  = (yVals.size - 1 - yi) * h  // flip: high values at top

            drawRect(fill, topLeft = Offset(left, top), size = Size(w - 1f, h - 1f))
            drawRect(
                axis.copy(alpha = 0.2f),
                topLeft = Offset(left, top),
                size = Size(w - 1f, h - 1f),
                style = Stroke(0.5f)
            )
        }
    }
}

/** Maps t ∈ [0,1] → colour: 0=red, 0.5=amber, 1=green */
private fun gainColor(t: Float): Color {
    return when {
        t < 0.5f -> {
            val s = t * 2f
            Color(red = 0.9f, green = 0.3f + s * 0.5f, blue = 0.1f)
        }
        else -> {
            val s = (t - 0.5f) * 2f
            Color(red = 0.9f - s * 0.7f, green = 0.8f, blue = 0.1f + s * 0.1f)
        }
    }
}

// ─── Line / sensitivity chart ─────────────────────────────────────────────────

/**
 * Shows gain vs. [xAxis] with a separate line for each value of [treatmentAxis].
 * Remaining axes are fixed to [fixed].
 */
@Composable
fun SensitivityLineChart(
    result:        RangeResult,
    xAxis:         RangeAxis,
    treatmentAxis: RangeAxis,
    fixed:         Map<RangeAxis, Int>,
    modifier:      Modifier = Modifier
) {
    val lineData = result.lineData(xAxis, treatmentAxis, fixed)
    if (lineData.isEmpty()) {
        Text("No data – adjust ranges.", modifier = modifier)
        return
    }

    val colors = listOf(
        Color(0xFF2E7D32), Color(0xFF1565C0), Color(0xFFE65100),
        Color(0xFF6A1B9A), Color(0xFF00838F)
    )

    val allGains = lineData.values.flatten().map { it.second }
    val minG = allGains.minOrNull() ?: 0.0
    val maxG = allGains.maxOrNull() ?: 1.0
    val range = if (abs(maxG - minG) < 1e-9) 1.0 else maxG - minG

    val onSurface = MaterialTheme.colorScheme.onSurface
    val xVals = result.axisValues(xAxis)

    Column(modifier) {
        Text(
            "Gain vs ${xAxis.label}  (treatment: ${treatmentAxis.label})",
            style = MaterialTheme.typography.titleSmall
        )
        Spacer(Modifier.height(4.dp))

        // Legend
        Row(horizontalArrangement = Arrangement.spacedBy(12.dp),
            modifier = Modifier.padding(bottom = 4.dp)) {
            lineData.keys.forEachIndexed { i, key ->
                Row(horizontalArrangement = Arrangement.spacedBy(4.dp)) {
                    Canvas(Modifier.size(10.dp).align(androidx.compose.ui.Alignment.CenterVertically)) {
                        drawCircle(colors[i % colors.size])
                    }
                    Text("${treatmentAxis.label}=$key",
                        style = MaterialTheme.typography.labelSmall)
                }
            }
        }

        Canvas(
            modifier = Modifier.fillMaxWidth().height(160.dp)
        ) {
            val pad = 36f; val padR = 8f; val padT = 8f; val padB = 20f
            val cW = size.width - pad - padR
            val cH = size.height - padT - padB

            fun yPx(g: Double) = padT + cH - ((g - minG) / range * cH).toFloat()
            fun xPx(idx: Int)  = pad + (if (xVals.size < 2) cW / 2 else
                xVals.indexOf(idx).toFloat() / (xVals.size - 1) * cW)

            // Grid
            for (k in 0..4) {
                val yp = yPx(minG + k * range / 4)
                drawLine(onSurface.copy(alpha = 0.12f),
                    Offset(pad, yp), Offset(size.width - padR, yp), 1f)
            }

            lineData.entries.forEachIndexed { li, (_, pts) ->
                val col = colors[li % colors.size]
                val screen = pts.map { (x, g) -> Offset(xPx(x), yPx(g)) }
                for (j in 1 until screen.size) {
                    drawLine(col, screen[j - 1], screen[j], strokeWidth = 3f)
                }
                screen.forEach { drawCircle(col, 5f, it) }
            }

            drawLine(onSurface.copy(alpha = 0.5f),
                Offset(pad, padT), Offset(pad, size.height - padB), 1.5f)
        }

        Row(Modifier.fillMaxWidth().padding(start = 32.dp, end = 8.dp),
            horizontalArrangement = Arrangement.SpaceAround) {
            xVals.forEach { v ->
                Text("$v", style = MaterialTheme.typography.labelSmall,
                    textAlign = TextAlign.Center, modifier = Modifier.weight(1f))
            }
        }
        Text(xAxis.label, style = MaterialTheme.typography.labelSmall,
            textAlign = TextAlign.Center, modifier = Modifier.fillMaxWidth())
    }
}
