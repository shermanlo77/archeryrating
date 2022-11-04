# GNU General Public License v3.0
# Copyright (c) 2019-2020 Sherman Lo

clean_names <- function(names) {
  for (i_name in seq_len(length(names))) {
    name <- names[i_name]
    if (stri_enc_toascii(name) == "Gas\032\032mova Ozay") {
      name <- "Gasimova Ozay"
    }
    nameLower <- tolower(name)
    names[i_name] <- switch(nameLower,
      ### MEN'S RECURVE
      "valladont jean-charles valentin" = substr(name, 1, 22),
      "van den berg sjef adrianus jozef" = substr(name, 1, 17),
      "wijler steve christina hendrikus" = substr(name, 1, 12),
      "huston patrick arthur" = substr(name, 1, 14),
      "hall thomas howard" = paste(substr(name, 1, 4), "Tom"),
      "wise alexander james" = substr(name, 1, 14),
      "o'brien tejada willian alberth" = paste(substr(name, 1, 22), "Albert"),
      "chirault thomas gino michel jerome" = substr(name, 1, 15),
      "van tongeren hendrikus jan" = paste(substr(name, 1, 12), substr(name, 24, 26)),
      "sierakowski kacper pawel" = substr(name, 1, 18),
      "plihon pierre jean abel" = substr(name, 1, 13),
      "ortegon morales lester alejandro" =
        gsub("GON", "GÓN", gsub("gon", "gón", name, fixed = TRUE), fixed = TRUE),
      "oona maert" = paste(substr(name, 1, 4), "Mart"),
      "martin chayse" =
        gsub("TIN", "TIN-ROBERTS", gsub("tin", "tin-roberts", name, fixed = TRUE), fixed = TRUE),
      "liahusheu alexander" = paste(substr(name, 1, 9), "aliaksandr"),
      "islam mohammad tamimul" =
        gsub("MOHAMMAD", "Md", gsub("Mohammad", "Md", name, fixed = TRUE), fixed = TRUE),
      "henckels jeff jean" = substr(name, 1, 13),
      "flink ludvig elof erling" = substr(name, 1, 12),
      "castro barcala daniel" = paste(substr(name, 1, 6), substr(name, 16, 21)),
      "bhoge yashdeep sanjay" = substr(name, 1, 14),
      "basiuras michal" = gsub("l", "ł", name, fixed = TRUE),
      "banchev ivan dimitrov" = substr(name, 1, 12),
      ### WOMEN'S RECURVE
      "zyzanska sylwia maria" = substr(name, 1, 15),
      "zejnullahu ardita" = gsub("J", "", gsub("j", "", name, fixed = TRUE), fixed = TRUE),
      "valencia trujillo alejandra" = paste(substring(name, 1, 8), substring(name, 19, 27)),
      "richter elena luise senta" = substring(name, 1, 13),
      "reidy maeve ita" = substring(name, 1, 11),
      "pitman bryony michaela" = substring(name, 1, 13),
      "perova kseniia" = gsub("ii", "i", name, fixed = TRUE),
      "pavlova anastasiia" = gsub("ii", "i", name, fixed = TRUE),
      "marin martinez alicia" = paste(substring(name, 1, 5), substring(name, 16, 21)),
      "laursen anne marie dorscheus" = substring(name, 1, 18),
      "jung amy nicole" = substring(name, 1, 8),
      "jager maja buskbjerg" = substring(name, 1, 10),
      "giaccheri tanya giada" = substring(name, 1, 15),
      "folkard naomi anne" = substring(name, 1, 13),
      "danailova dobromira yordanova" = substring(name, 1, 19),
      "coskun gulnaz busranur" = substring(name, 1, 13),
      "castanos bornaechea celia" = paste(substring(name, 1, 8), substring(name, 21, 25)),
      "bjerendal christine louise" = substring(name, 1, 19),
      "bettles sarah louise anne" = substring(name, 1, 13),
      "bayardo chan ana gabriela" = paste(substring(name, 1, 7), substring(name, 18, 25)),
      "baria premilaben shankarbhai" = substring(name, 1, 16),
      "amaiestroaie madalina" =
        gsub("EST", "ST", gsub("est", "st", name, fixed = TRUE), fixed = TRUE),
      "tomomi sugimoto" = paste(substring(
        gsub("Tomomi", "Sugimoto", gsub("TOMOMI", "SUGIMOTO", name, fixed = TRUE), fixed = TRUE),
        1, 8
      ), "Tomomi"),
      ### MEN'S COMPOUND
      "taylor stuart jonathon" = substring(name, 1, 13),
      "schloesser michael bastiaan lucas wilh" = paste(substring(name, 1, 10), "Mike"),
      "ravenscroft adam philip" = substring(name, 1, 16),
      "przybylski lukasz slawomir" = substring(name, 1, 17),
      "nolasco carias douglas vladimir" = paste(substring(name, 1, 7), substring(name, 16, 31)),
      "nedeljkovic ognjen" = gsub("C", "Ć", gsub("c", "ć", name, fixed = TRUE), fixed = TRUE),
      "hollas paul leon" = paste(substring(name, 1, 6), substring(name, 13, 16)),
      "gonzalez de alba rodolfo" = paste(substring(name, 1, 8), substring(name, 18, 24)),
      "deloche pierre-julien antoine amaury" = substring(name, 1, 21),
      "borgstroem hampus zacharias" = substring(name, 1, 17),
      ### WOMEN'S COMPOUND
      "moon sarah elizabeth" = substring(name, 1, 10),
      "merino escudero brenda" = paste(substring(name, 1, 6), substring(name, 17, 22)),
      "mat salleh fatin nurfatehah" = gsub("MAT Salleh", "MAT SALLEH", name),
      "mason lucy torrin" = substring(name, 1, 10),
      "marcos garcia andrea" = paste(substring(name, 1, 6), substring(name, 15, 20)),
      "jensen tanja kirstine amdi" = substring(name, 1, 12),
      "dodemont sophie denise paulette" = substring(name, 1, 15),
      "de laat sanne josephina adriana" = substring(name, 1, 13),
      "cox cassidy louise" = substring(name, 1, 11),
      "becerra arizaga andrea maya" = paste(substring(name, 1, 7), substring(name, 17, 22)),
      "alvarez ospina m. pilar" = paste(substring(name, 1, 14), substring(name, 19, 23)),
      ### DEFAULT
      name
    )
  }

  return(names)
}
