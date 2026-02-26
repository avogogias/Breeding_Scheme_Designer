package com.eib.breedingdesigner.simulation

import com.eib.breedingdesigner.model.*
import kotlin.math.max
import kotlin.math.roundToInt

/**
 * Runs the breeding simulation over a 4-D grid of first-stage parameters
 * (entries × years × locs × reps), matching the behaviour of the R Shiny
 * Ranges tab / runScenarioRange_r() function.
 *
 * Only the FIRST breeding stage has its parameters swept; all subsequent stages
 * are fixed.  Combinations where the first-stage entry count is not strictly
 * greater than the next stage are silently skipped (the R app does not validate
 * this, but the C++ engine would index out-of-bounds).
 */
object RangesSimulator {

    // ─── Public API ───────────────────────────────────────────────────────────

    fun run(config: RangesConfig): RangeResult {
        val entriesVals = rangeGrain(config.entriesRange)
        val yearsVals   = rangeGrain(config.yearsRange)
        val locsVals    = rangeGrain(config.locsRange)
        val repsVals    = rangeGrain(config.repsRange)

        val points = mutableListOf<RangePoint>()

        for (entries in entriesVals) {
            // Skip if first-stage entries do not exceed the next fixed stage
            val nextStageEntries = config.fixedStages.firstOrNull()?.entries ?: 0
            if (entries <= nextStageEntries) continue

            for (years in yearsVals) {
                for (locs in locsVals) {
                    for (reps in repsVals) {
                        val pt = simulate(config, entries, years, locs, reps) ?: continue
                        points.add(pt)
                    }
                }
            }
        }

        return RangeResult(
            config        = config,
            points        = points,
            entriesValues = entriesVals.filter { it > (config.fixedStages.firstOrNull()?.entries ?: 0) },
            yearsValues   = yearsVals,
            locsValues    = locsVals,
            repsValues    = repsVals
        )
    }

    // ─── Pure helpers (also accessible for tests) ─────────────────────────────

    /**
     * Generates [samples] evenly-spaced integer values from [param.min] to
     * [param.max] (both inclusive).
     *
     * Equivalent to R's rangeGrain(min, max, grain) used in server.r.
     */
    fun rangeGrain(param: RangeParam): List<Int> {
        if (param.min >= param.max) return listOf(param.min)
        val n = param.samples
        return (0 until n).map { i ->
            (param.min + i.toDouble() * (param.max - param.min) / (n - 1)).roundToInt()
        }.distinct()
    }

    /** Overload that accepts raw min/max/samples for test convenience. */
    fun rangeGrain(min: Int, max: Int, samples: Int): List<Int> =
        rangeGrain(RangeParam(min = max(1, min), max = max(max(1, min), max), samples = max(2, samples)))

    // ─── Private simulation runner ────────────────────────────────────────────

    private fun simulate(
        config:  RangesConfig,
        entries: Int,
        years:   Int,
        locs:    Int,
        reps:    Int
    ): RangePoint? {
        // Build first stage from current grid point
        val firstStage = Stage(
            entries       = entries,
            years         = years,
            locs          = locs,
            reps          = reps,
            errorVariance = config.errorVariance,
            plotCost      = config.plotCost,
            locCost       = config.locCost,
            fixedCost     = config.fixedCost
        )

        // Combine with the fixed subsequent stages
        val allStages = listOf(firstStage) + config.fixedStages

        // Validate that final varieties < last-stage entries
        if (config.varieties >= allStages.last().entries) return null

        val params = SimulationParams(
            varG      = config.varG,
            varGxL    = config.varGxL,
            varGxY    = config.varGxY,
            entries   = allStages.map { it.entries },
            years     = allStages.map { it.years },
            locs      = allStages.map { it.locs },
            reps      = allStages.map { it.reps },
            error     = allStages.map { it.errorVariance },
            varieties = config.varieties,
            nRepeats  = config.nRepeats,
            multYears = config.multYears,
            plotCosts  = allStages.map { it.plotCost },
            locCosts   = allStages.map { it.locCost },
            fixedCosts = allStages.map { it.fixedCost }
        )

        val result      = BreedingSimulator.run(params)
        val finalStats  = result.stageStats.last()
        val totalYears  = config.multYears + allStages.sumOf { it.years }
        val totalCost   = params.totalCost()

        return RangePoint(
            entries     = entries,
            years       = years,
            locs        = locs,
            reps        = reps,
            meanGain    = finalStats.mean,
            sdGain      = finalStats.sd,
            gainPerYear = if (totalYears > 0) finalStats.mean / totalYears else 0.0,
            gainPerCost = if (totalCost  > 0) finalStats.mean / totalCost  else 0.0
        )
    }
}
