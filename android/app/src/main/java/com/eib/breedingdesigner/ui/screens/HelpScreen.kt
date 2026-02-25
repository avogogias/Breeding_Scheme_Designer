package com.eib.breedingdesigner.ui.screens

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material3.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp

@Composable
fun HelpScreen() {
    Column(
        modifier = Modifier
            .fillMaxSize()
            .verticalScroll(rememberScrollState())
            .padding(16.dp),
        verticalArrangement = Arrangement.spacedBy(16.dp)
    ) {
        Text("About", style = MaterialTheme.typography.headlineSmall, color = MaterialTheme.colorScheme.primary)

        HelpCard("What is this app?") {
            Text(
                "Breeding Scheme Designer helps plant breeders simulate and compare different " +
                "breeding program strategies. It calculates expected genetic gain using deterministic " +
                "simulation, enabling you to explore trade-offs between evaluation strategies, " +
                "resource allocation, and selection intensity.",
                style = MaterialTheme.typography.bodyMedium
            )
        }

        HelpCard("How to use") {
            listOf(
                "1. Configure variances (σ²G, σ²GxL, σ²GxY) to match your crop.",
                "2. Set the number of multiplication years (pre-testing phase).",
                "3. Define your yield trial stages – entries, years, locations, reps.",
                "4. Optionally set plot/location/fixed costs per stage.",
                "5. Choose the number of final varieties to select.",
                "6. Press Run Simulation to compute results.",
                "7. View box plots of genetic gain on the Results tab.",
                "8. Add more scenarios in Compare to explore alternatives."
            ).forEach { step ->
                Text(step, style = MaterialTheme.typography.bodyMedium, modifier = Modifier.padding(vertical = 2.dp))
            }
        }

        HelpCard("Glossary") {
            val terms = mapOf(
                "σ²G (Genetic Variance)" to "Variance among true breeding values of entries.",
                "σ²GxL (GxL Variance)" to "Variance due to Genotype-by-Location interaction (within year).",
                "σ²GxY (GxY Variance)" to "Variance due to Genotype-by-Year interaction.",
                "Plot Error Variance" to "Residual variance not explained by genetic or interaction effects.",
                "h² (Heritability)" to "Proportion of phenotypic variance attributable to genetic variance. Higher h² means more reliable selection.",
                "Entries" to "Number of germplasm lines evaluated at a stage.",
                "Multiplication Years" to "Years of selfing or multiplication before formal yield trials.",
                "Genetic Gain" to "Expected improvement in mean genetic value after selection.",
                "Gain/Year" to "Total genetic gain divided by total program years (including multiplication).",
                "Gain/Cost" to "Total genetic gain divided by total program cost."
            )
            terms.forEach { (term, def) ->
                Text(term, style = MaterialTheme.typography.labelLarge, color = MaterialTheme.colorScheme.primary,
                    modifier = Modifier.padding(top = 8.dp))
                Text(def, style = MaterialTheme.typography.bodySmall)
            }
        }

        HelpCard("Credits") {
            Text(
                "Algorithm based on the Breeding Scheme Designer R Shiny app by the " +
                "Excellence in Breeding (EiB) platform.\n" +
                "Original R/C++ code by Thanasis Vogogias & Chris Gaynor.\n" +
                "Android port: Jetpack Compose + Kotlin.",
                style = MaterialTheme.typography.bodySmall
            )
        }
    }
}

@Composable
private fun HelpCard(title: String, content: @Composable ColumnScope.() -> Unit) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        elevation = CardDefaults.cardElevation(2.dp)
    ) {
        Column(Modifier.padding(16.dp), verticalArrangement = Arrangement.spacedBy(4.dp)) {
            Text(title, style = MaterialTheme.typography.titleMedium, color = MaterialTheme.colorScheme.primary)
            Divider()
            content()
        }
    }
}
