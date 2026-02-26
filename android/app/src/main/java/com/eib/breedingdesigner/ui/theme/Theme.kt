package com.eib.breedingdesigner.ui.theme

import androidx.compose.material3.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.graphics.Color

// Agricultural green palette
val GreenPrimary      = Color(0xFF2E7D32)
val GreenOnPrimary    = Color(0xFFFFFFFF)
val GreenContainer    = Color(0xFFC8E6C9)
val GreenOnContainer  = Color(0xFF1B5E20)
val GreenSecondary    = Color(0xFF558B2F)
val GreenTertiary     = Color(0xFFF9A825)
val SurfaceVariant    = Color(0xFFF1F8E9)
val Background        = Color(0xFFFAFAF5)
val ErrorColor        = Color(0xFFB71C1C)

private val LightColorScheme = lightColorScheme(
    primary          = GreenPrimary,
    onPrimary        = GreenOnPrimary,
    primaryContainer = GreenContainer,
    onPrimaryContainer = GreenOnContainer,
    secondary        = GreenSecondary,
    tertiary         = GreenTertiary,
    background       = Background,
    surface          = Background,
    surfaceVariant   = SurfaceVariant,
    error            = ErrorColor
)

@Composable
fun BreedingDesignerTheme(content: @Composable () -> Unit) {
    MaterialTheme(
        colorScheme = LightColorScheme,
        typography   = Typography(),
        content      = content
    )
}
