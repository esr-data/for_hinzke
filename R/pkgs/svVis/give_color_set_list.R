

give_color_set_list <- function(){

  # browser()

  return(
    list(

      # SV main (sv0) ----------------------------------------------------------

      sv0_allmain = c(
        "#195365", "#6896A8", "#ADA58B", "#91BEA0", "#B5BFC5",
        "#E73F0C", "#9650EB", "#AFD700"
      ),

      # scales (positiv --> negativ)

      sv0_skala_posneg_2 = c(
        "#91BEA0",  "#6896A8"
      ),

      sv0_skala_posneg_3 = c(
        "#91BEA0", "#ADA58B", "#6896A8"
      ),

      sv0_skala_posneg_4 = c(
        "#91BEA0", "#A2CEB1", "#79A8BA","#6896A8"
      ),

      sv0_skala_posneg_5 = c(
        "#91BEA0", "#A2CEB1", "#ADA58B", "#79A8BA","#6896A8"
      ),

      sv0_skala_posneg_6 = c(
        "#91BEA0", "#A2CEB1", "#B3DFC2", "#8AB9CC", "#7AA9BA","#6896A8"
      ),

      sv0_skala_posneg_7 = c(
        "#91BEA0", "#A2CEB1", "#B3DFC2", "#ADA58B",  "#8AB9CC", "#7AA9BA","#6896A8"
      ),

      # gradients

      sv0_mono_rot = c(
        "#E73f0C", "#F9D4C8"
      ),

      sv0_mono_eisblau = c(
        "#6896A8", "#DDE7EB"
      ),

      sv0_mono_dunkelblau = c(
        "#195365", "#CBD8DC"
      ),

      sv0_mono_svgold = c(
        "#ADA58B", "#ECEBE5"
      ),

      sv0_mono_mint = c(
        "#91BEA0", "#E6F0E9"
      ),

      # SV education & skills (sv1) --------------------------------------------

      sv1_allmain = c(
        "#195365", "#AFD700", "#6896A8", "#91BEA0", "#B5BFC5", "#ADA58B",
        "#E73f0C", "#9650EB"
      ),

      # scales (positiv --> negativ)

      sv1_skala_posneg_2 = c(
        "#AFD700","#6896A8"
      ),

      sv1_skala_posneg_3 = c(
        "#AFD700", "#ADA58B", "#6896A8"
      ),

      sv1_skala_posneg_4 = c(
        "#AFD700","#C1E900", "#79A8BA","#6896A8"
      ),

      sv1_skala_posneg_5 = c(
        "#AFD700","#C1E900", "#ADA58B", "#79A8BA","#6896A8"
      ),

      sv1_skala_posneg_6 = c(
        "#AFD700","#C1E900", "#D3FF20", "#8AB9CC", "#7AA9BA","#6896A8"
      ),

      sv1_skala_posneg_7 = c(
        "#AFD700","#C1E900", "#D3FF20", "#ADA58B", "#8AB9CC", "#7AA9BA","#6896A8"
      ),

      # scales (negativ --> positiv)

      sv1_skala_negpos_2 = c(
        "#6896A8", "#AFD700"
      ),

      sv1_skala_negpos_3 = c(
        "#6896A8", "#ADA58B", "#AFD700"
      ),

      sv1_skala_negpos_4 = c(
        "#6896A8", "#79A8BA", "#C1E900", "#AFD700"
      ),

      sv1_skala_negpos_5 = c(
        "#6896A8", "#79A8BA", "#ADA58B", "#C1E900", "#AFD700"
      ),

      sv1_skala_negpos_6 = c(
        "#6896A8", "#7AA9BA", "#8AB9CC", "#D3FF20", "#C1E900", "#AFD700"
      ),

      sv1_skala_negpos_7 = c(
        "#6896A8", "#7AA9BA", "#8AB9CC", "#ADA58B", "#D3FF20", "#C1E900", "#AFD700"
      ),

      # gradiants

      sv1_mono_gift = c(
        "#AFD700", "#EDF6C6"
      ),

      # SV collaborative research & innovation (sv2) ---------------------------

      sv2_allmain = c(
        "#195365", "#9650EB", "#6896A8", "#91BEA0", "#B5BFC5", "#ADA58B",
        "#E73f0C", "#AFD700"
      ),

      # scales (positiv --> negativ)

      sv2_skala_posneg_2 = c(
        "#91BEA0", "#9650EB"
      ),

      sv2_skala_posneg_3 = c(
        "#91BEA0",  "#ADA58B", "#9650EB"
      ),

      sv2_skala_posneg_4 = c(
        "#91BEA0", "#A2CEB1", "#B488FF", "#9650EB"
      ),

      sv2_skala_posneg_5 = c(
        "#91BEA0", "#A2CEB1", "#ADA58B","#B488FF", "#9650EB"
      ),

      sv2_skala_posneg_6 = c(
        "#91BEA0", "#A2CEB1", "#B3DFC2","#D2AFFF","#B488FF", "#9650EB"
      ),

      sv2_skala_posneg_7 = c(
        "#91BEA0", "#A2CEB1", "#B3DFC2", "#ADA58B","#D2AFFF","#B488FF", "#9650EB"
      ),

      # gradients

      sv2_mono_lila = c(
        "#9650EB",  "#E7D8FA"
      ),

      # KI-Campus (kic) --------------------------------------------------------

      kic_001 = c(
        "#3A2A78", "#1AA469", "#2891BA", "#EAF4F8", "#FFFFFF",
        "#000000", "#E2DE00", "#FF737E", "#F39200", "#F9CAC2",
        "#AE82B9", "#00757C", "#84A6B0", "#C42785", "#6DC7DC",
        "#D1C1A3"
      ),

      # scales

      kic_002 = c(
         "#FF737E", "#F9CAC2","#84A6B0", "#2891BA", "#1AA469"
      ),

      # MINT-Vernetzt (mnt) ----------------------------------------------------

      mnt_001 = c(
        "#154194", "#EFE8e6", "#66CBAF", "#B16FAB", "#DCBED9",
        "#D0A9CD", "#BE88BA", "#ECDBEA", "#F1EBEA", "#BBD1FC",
        "#5F94F9", "#1B54C0", "#2D6BE1", "#EDF3FF", "#DDFFF6",
        "#AFF3E0", "#35BD97", "#F4EFEE", "#EFE8E6", "#D4C1BB"
      ),

      # scales

      mnt_002 = c(
        "#66CBAF", "#B16FAB", "#EFE8E6"
      ),

      # colorful chaos

      mnt_chaos =  c(
        "#B16FAB", "#154194", "#66CBAF", "#FBBF24", "#8893A7",
        "#EE7775", "#9D7265", "#35BD97", "#5D335A", "#BFC6D3",
        "#5F94F9", "#B45309", "#007655", "#FDE68a", "#DC2626",
        "#D4C1bb", "#D0A9CD", "#FCA5A5", "#112C5F"
      ),

      # HFD (hfd) --------------------------------------------------------------

      hfd_001 = c(
        "#003A7B", "#0055AD", "#00D6E8", "#000000"
      ),

      # gradients

      hfd_mono_dark = c(
        "#003A7B",  "#00D6E8"
      )
    )
  )
}
