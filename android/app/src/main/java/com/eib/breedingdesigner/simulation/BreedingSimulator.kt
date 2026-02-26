package com.eib.breedingdesigner.simulation

import com.eib.breedingdesigner.model.SimulationParams
import com.eib.breedingdesigner.model.SimulationResult
import com.eib.breedingdesigner.model.StageStats
import java.util.Random
import kotlin.math.sqrt

/**
 * Kotlin port of Engine.cpp – deterministic simulation of breeding scheme genetic gain.
 *
 * Algorithm (mirrors runScenario / runScenarioLite):
 *  For each replicate:
 *    1. Sample genetic values g ~ N(0, varG) for entries[0] candidates.
 *    2. Stage 0: add non-genetic noise, compute phenotype p = g + e.
 *    3. Stages 1..nStages-1:
 *         - Rank p descending, keep top entries[s] candidates.
 *         - Record mean(g[0..varieties-1]) as genetic gain at stage s-1.
 *         - Compute new phenotype for stage s.
 *    4. Final stage: rank p, keep top 'varieties'; record mean genetic value.
 *  Summarise across replicates → mean and SD per stage.
 */
object BreedingSimulator {

    fun run(params: SimulationParams): SimulationResult {
        val nStages = params.entries.size
        // rawOutput[stage][replicate] = mean genetic value of best varieties at that stage
        val rawOutput = Array(nStages) { DoubleArray(params.nRepeats) }

        val rng = Random(System.currentTimeMillis())

        repeat(params.nRepeats) { rep ->
            // Initial genetic values ~ N(0, varG)
            var g = DoubleArray(params.entries[0]) { rng.nextGaussian() * sqrt(params.varG) }

            // Stage 0 non-genetic variance and phenotype
            var p = phenotype(g, params, 0, rng)

            // Stages 1 to nStages-1
            for (s in 1 until nStages) {
                // Sort p descending; keep top entries[s]
                val selectedIdx = topIndices(p, params.entries[s])

                // Update g to selected candidates (order preserved from sort = best first)
                g = DoubleArray(params.entries[s]) { i -> g[selectedIdx[i]] }

                // Report mean of top 'varieties' entries (already sorted best-first)
                val nReport = minOf(params.varieties, g.size)
                rawOutput[s - 1][rep] = g.take(nReport).average()

                // New phenotype for stage s
                p = phenotype(g, params, s, rng)
            }

            // Final selection: pick top 'varieties' by phenotype
            val finalIdx = topIndices(p, params.varieties)
            rawOutput[nStages - 1][rep] = finalIdx.map { g[it] }.average()
        }

        val stageStats = (0 until nStages).map { s ->
            computeStats(rawOutput[s])
        }

        val finalMean = stageStats.last().mean
        val totalYears = params.years.sum() + params.multYears
        val totalCost = params.totalCost()

        return SimulationResult(
            stageStats = stageStats,
            totalGain = finalMean,
            gainPerYear = if (totalYears > 0) finalMean / totalYears else 0.0,
            gainPerCost = if (totalCost > 0) finalMean / totalCost else 0.0,
            rawOutput = rawOutput
        )
    }

    // ---------- helpers ----------

    private fun phenotype(g: DoubleArray, p: SimulationParams, stage: Int, rng: Random): DoubleArray {
        val y = p.years[stage].toDouble()
        val l = p.locs[stage].toDouble()
        val r = p.reps[stage].toDouble()
        val e = p.error[stage]
        val w = p.varGxL / (l * y) + p.varGxY / y + e / (r * l * y)
        val sdNonG = sqrt(maxOf(w, 0.0))
        return DoubleArray(g.size) { i -> g[i] + rng.nextGaussian() * sdNonG }
    }

    /** Returns indices sorted by phenotype descending, first [n] elements. */
    private fun topIndices(p: DoubleArray, n: Int): List<Int> =
        p.indices.sortedByDescending { p[it] }.take(n)

    private fun computeStats(values: DoubleArray): StageStats {
        val mean = values.average()
        val variance = values.sumOf { (it - mean) * (it - mean) } / values.size
        val sd = sqrt(variance)
        val sorted = values.sorted()
        return StageStats(
            mean = mean,
            sd = sd,
            median = percentile(sorted, 50.0),
            q25 = percentile(sorted, 25.0),
            q75 = percentile(sorted, 75.0),
            min = sorted.first(),
            max = sorted.last()
        )
    }

    private fun percentile(sorted: List<Double>, p: Double): Double {
        if (sorted.isEmpty()) return 0.0
        val idx = (p / 100.0) * (sorted.size - 1)
        val lo = sorted[idx.toInt()]
        val hi = sorted[minOf(idx.toInt() + 1, sorted.size - 1)]
        return lo + (hi - lo) * (idx - idx.toInt())
    }
}
