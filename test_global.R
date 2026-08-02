remove.packages("CausalMultiOmics")

devtools::document()
devtools::install()
devtools::check()


#PROBAMOS CLASES

library(CausalMultiOmics)

data <- MultiOmicsData()
result <- CMOResult()
class(data)
class(result)
str(data)
str(result)
devtools::load_all()
MultiOmicsData()
CMOResult()

#PROBAMOS MÉTODOS

x <- MultiOmicsData()
x

summary(x)

y <- CMOResult()
y

summary(y)

#PROBAMOS FUNCIÓN LOAD_DATA

transcriptomics <- data.frame(
  Gene1 = rnorm(10),
  Gene2 = rnorm(10),
  row.names = paste0("S", 1:10)
)

proteomics <- data.frame(
  Protein1 = rnorm(8),
  Protein2 = rnorm(8),
  row.names = paste0("S", 3:10)
)

obj <- load_data(

  assays = list(

    transcriptomics = transcriptomics,

    proteomics = proteomics

  )

)

str(obj, max.level = 2)

print(obj)
summary(obj)

# =============================================================================
# PROBAMOS LA CLASE CMOValidation
# =============================================================================

validation <- CMOValidation()

class(validation)
str(validation)

print(validation)
summary(validation)


# =============================================================================
# PROBAMOS check_data()
# =============================================================================

validation <- check_data(obj)

class(validation)
str(validation, max.level = 2)

print(validation)
summary(validation)


# =============================================================================
# COMPROBAMOS EL RESULTADO
# =============================================================================

validation$valid

validation$errors

validation$warnings

validation$summary

validation$summary$block_summary

validation$summary$overlap

validation$details


# =============================================================================
# CASO CON METADATA
# =============================================================================

metadata <- data.frame(

  sample_id = paste0("S", 1:10),

  group = rep(c("Control", "Case"), each = 5),

  age = sample(30:70, 10),

  stringsAsFactors = FALSE

)

obj2 <- load_data(

  assays = list(

    transcriptomics = transcriptomics,

    proteomics = proteomics

  ),

  metadata = metadata

)

validation2 <- check_data(obj2)

print(validation2)
summary(validation2)

validation2$details

validation2$details$block_only
validation2$details$metadata_only


# =============================================================================
# CASO CON VALORES PERDIDOS
# =============================================================================

transcriptomics_na <- transcriptomics

transcriptomics_na[1,1] <- NA
transcriptomics_na[3,2] <- NA
transcriptomics_na[5,2] <- NA

obj3 <- load_data(

  assays = list(

    transcriptomics = transcriptomics_na,

    proteomics = proteomics

  )

)

validation3 <- check_data(obj3)

summary(validation3)

validation3$details$missing_values


# =============================================================================
# CASO CON VARIABLE CONSTANTE
# =============================================================================

proteomics_constant <- proteomics

proteomics_constant$Constant <- 1

obj4 <- load_data(

  assays = list(

    transcriptomics = transcriptomics,

    proteomics = proteomics_constant

  )

)

validation4 <- check_data(obj4)

summary(validation4)

validation4$details$constant_features


# =============================================================================
# CASO CON VARIABLE CASI CONSTANTE
# =============================================================================

proteomics_near <- proteomics

proteomics_near$NearConstant <- c(rep(1, 7), 2)

obj4b <- load_data(

  assays = list(

    transcriptomics = transcriptomics,

    proteomics = proteomics_near

  )

)

validation4b <- check_data(obj4b)

summary(validation4b)

validation4b$details$near_constant_features


# =============================================================================
# CASO CON MUESTRAS VACÍAS
# =============================================================================

proteomics_empty <- proteomics

proteomics_empty[2,] <- NA

obj5 <- load_data(

  assays = list(

    transcriptomics = transcriptomics,

    proteomics = proteomics_empty

  )

)

validation5 <- check_data(obj5)

summary(validation5)

validation5$details$empty_samples


# =============================================================================
# CASO CON VARIABLES VACÍAS
# =============================================================================

proteomics_empty_var <- proteomics

proteomics_empty_var$Empty <- NA

obj6 <- load_data(

  assays = list(

    transcriptomics = transcriptomics,

    proteomics = proteomics_empty_var

  )

)

validation6 <- check_data(obj6)

summary(validation6)

validation6$details$empty_features


# =============================================================================
# CASO CON NOMBRES DUPLICADOS (DEBE FALLAR EN load_data)
# =============================================================================

dup <- transcriptomics

rownames(dup)[2] <- rownames(dup)[1]

try(

  load_data(

    assays = list(

      transcriptomics = dup

    )

  )

)


# =============================================================================
# CASO CON VARIABLES DUPLICADAS (DEBE FALLAR EN load_data)
# =============================================================================

dup2 <- transcriptomics

colnames(dup2)[2] <- colnames(dup2)[1]

try(

  load_data(

    assays = list(

      transcriptomics = dup2

    )

  )

)

