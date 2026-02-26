package com.eib.breedingdesigner.ui.components

import androidx.compose.foundation.Canvas
import androidx.compose.foundation.background
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
import com.eib.breedingdesigner.model.ScenarioResult
import kotlin.math.abs

/** Which metric variant to display in GroupedBoxPlotChart. */
enum class GroupMetric(val chartTitle: String) {
    GAIN("Genetic Gain by Stage"),
    GAIN_PER_YEAR("Gain per Year by Stage"),
    GAIN_PER_COST("Gain per Cost by Stage")
}

/** Palette – up to 5 scenarios, matching CompareChart colours. */
val ScenarioPalette = listOf(
    Color(0xFF2E7D32),
    Color(0xFF1565C0),
    Color(0xFFE65100),
    Color(0xFF6A1B9A),
    Color(0xFF00838F)
)

/**
 * Draws side-by-side box plots at each breeding stage, one coloured box
 * per scenario.  Mirrors the R app's plotScenarioGroup() / Overview tab.
 *
 * @param results  List of scenario results to compare (up to 5).
 * @param metric   Which of Gain / Gain-per-Year / Gain-per-Cost to show.
 */
@Composable
fun GroupedBoxPlotChart(
    results: List<ScenarioResult>,
    metric: GroupMetric,
    modifier: Modifier = Modifier
) {
    if (results.isEmpty()) return
    val maxStages = results.maxOf { it.result.stageStats.size }
    val onSurface = MaterialTheme.colorScheme.onSurface

    Column(modifier) {
        Text(
            metric.chartTitle,
            style = MaterialTheme.typography.titleSmall,
            modifier = Modifier.padding(bottom = 4.dp)
        )

        // Colour legend
        Row(
            horizontalArrangement = Arrangement.spacedBy(12.dp),
            modifier = Modifier.padding(bottom = 4.dp)
        ) {
            results.forEachIndexed { i, sr ->
                Row(
                    verticalAlignment = Alignment.CenterVertically,
                    horizontalArrangement = Arrangement.spacedBy(4.dp)
                ) {
                    Box(
                        Modifier
                            .size(10.dp)
                            .background(ScenarioPalette[i % ScenarioPalette.size])
                    )
                    Text(sr.scenario.name, style = MaterialTheme.typography.labelSmall)
                }
            }
        }

        Canvas(
            modifier = Modifier
                .fillMaxWidth()
                .height(200.dp)
        ) {
            drawGroupedBoxes(results, metric, maxStages, onSurface)
        }

        // X-axis stage labels
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .padding(horizontal = 8.dp),
            horizontalArrangement = Arrangement.SpaceAround
        ) {
            (1..maxStages).forEach { s ->
                Text(
                    "S$s",
                    style = MaterialTheme.typography.labelSmall,
                    textAlign = TextAlign.Center,
                    modifier = Modifier.weight(1f)
                )
            }
        }
    }
}

// ─── Canvas drawing ───────────────────────────────────────────────────────────

private fun DrawScope.drawGroupedBoxes(
    results: List<ScenarioResult>,
    metric: GroupMetric,
    maxStages: Int,
    axisColor: Color
) {
    val padL = 40f; val padR = 8f; val padT = 8f; val padB = 24f
    val chartW = size.width - padL - padR
    val chartH = size.height - padT - padB

    // Compute the scaling factor per scenario (divides each stage value)
    val scales: List<Double> = results.map { sr -> metricScale(sr, metric) }

    // Determine y-axis range across all scenarios × stages
    val allScaled = results.flatMapIndexed { i, sr ->
        sr.result.stageStats.flatMap { st ->
            listOf(st.min * scales[i], st.max * scales[i])
        }
    }
    val yMin = allScaled.minOrNull() ?: 0.0
    val yMax = allScaled.maxOrNull() ?: 1.0
    val yRange = if (abs(yMax - yMin) < 1e-9) 1.0 else yMax - yMin

    fun yPx(v: Double): Float =
        (padT + chartH - (v - yMin) / yRange * chartH).toFloat()

    // Grid lines
    val gridColor = axisColor.copy(alpha = 0.12f)
    for (k in 0..4) {
        val yp = yPx(yMin + k * yRange / 4.0)
        drawLine(gridColor, Offset(padL, yp), Offset(size.width - padR, yp), 1f)
    }

    val slotW = chartW / maxStages
    val n = results.size
    val halfW = slotW * 0.35f / n   // half-width of each box

    results.forEachIndexed { si, sr ->
        val color = ScenarioPalette[si % ScenarioPalette.size]
        val scale = scales[si]

        sr.result.stageStats.forEachIndexed { s, st ->
            // Centre x for this scenario within this stage's slot
            val cx = padL + slotW * s + slotW * (si + 0.5f) / n

            val q3px = yPx(st.q75 * scale)
            val q1px = yPx(st.q25 * scale)
            val medPx = yPx(st.median * scale)
            val minPx = yPx(st.min * scale)
            val maxPx = yPx(st.max * scale)

            // Whisker lines
            drawLine(color, Offset(cx, minPx), Offset(cx, q1px), strokeWidth = 1.8f)
            drawLine(color, Offset(cx, q3px), Offset(cx, maxPx), strokeWidth = 1.8f)
            // Whisker caps
            val capW = halfW * 0.5f
            drawLine(color, Offset(cx - capW, minPx), Offset(cx + capW, minPx), 1.8f)
            drawLine(color, Offset(cx - capW, maxPx), Offset(cx + capW, maxPx), 1.8f)

            // IQR box – fill then outline
            val boxH = q1px - q3px
            if (boxH > 0f) {
                drawRect(
                    color = color.copy(alpha = 0.22f),
                    topLeft = Offset(cx - halfW, q3px),
                    size = Size(halfW * 2, boxH)
                )
                drawRect(
                    color = color,
                    topLeft = Offset(cx - halfW, q3px),
                    size = Size(halfW * 2, boxH),
                    style = Stroke(width = 2f)
                )
            }

            // Median line (white so it shows on filled box)
            drawLine(Color.White, Offset(cx - halfW, medPx), Offset(cx + halfW, medPx), 2.5f)
        }
    }

    // Y-axis
    drawLine(
        axisColor.copy(alpha = 0.5f),
        Offset(padL, padT),
        Offset(padL, size.height - padB),
        strokeWidth = 1.5f
    )
}

/**
 * Returns a scalar that, when multiplied by a raw stage gain value, converts it
 * to the target metric (1.0 for raw Gain, 1/totalYears for Gain/Year, etc.).
 */
private fun metricScale(sr: ScenarioResult, metric: GroupMetric): Double = when (metric) {
    GroupMetric.GAIN -> 1.0
    GroupMetric.GAIN_PER_YEAR -> {
        val y = sr.scenario.totalYears
        if (y > 0) 1.0 / y else 1.0
    }
    GroupMetric.GAIN_PER_COST -> {
        val c = sr.scenario.totalCost
        if (c > 0) 1.0 / c else 1.0
    }
}
