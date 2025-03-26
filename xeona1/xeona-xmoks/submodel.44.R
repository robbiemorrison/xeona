#! /usr/bin/Rscript --vanilla

#  Purpose   : "hand" checking of AC transmission parameters
#  Author    : Robbie Morrison <robbie@actrix.co.nz>
#  Commenced : 13-Jan-2012
#  Status    : working
#  Keywords  : xeona ac-transmission

#  $Revision$
#  $Date$
#  $Author$
#  $RCSfile$

#---------------------------------
#  user modifable
#---------------------------------

submodel     <- "submodel.44.guard.xem"      # XEM filename
transmission <- "teas-ac-transmission-1"     # entity identifier

fmt <- "%+.3e"
fmt <- "%+.4g"

debug <- TRUE
debug <- FALSE

# ---------------------------------
#  preamble
# ---------------------------------

library(xem)
library(robbie)

# ---------------------------------
#  function : grab
#  function : transgrab
# ---------------------------------

grab <- function (fqf)
{
  if ( grepl("^entity\\.", fqf) == FALSE )
    {
      fqf <- paste("entity.", fqf, sep = "") # append "entity". if required
    }

  # somewhat involved because base file must contain
  # one dot in addition to the ".xem"

  tmpxem <- tempfile("xeona.")               # CAUTION: dot essential
  fulxem <- paste(tmpxem, "xem", sep = ".")
  file.copy(submodel, fulxem)
  xfqf   <- paste(tmpxem, fqf, sep = ".")
  value  <- xem.grab(xfqf, report = debug)
}

transgrab <- function(field)
{
  grab(paste(transmission, field, sep = "."))
}

# ---------------------------------
#  function : deg2rad
# ---------------------------------

deg2rad <- function(degrees)
{
  radians <- degrees * ( 2.0 * pi / 360.0 )
}

# ---------------------------------
#  function : calc
# ---------------------------------

calc <- function(A, B)
{
  val <- A /( A ^ 2 + B ^ 2 )
}

# ---------------------------------
#  active code 1
# ---------------------------------

script <- robbie.getScriptName()
message(script, " commencing")
message()

message("source                        : ", submodel)
message()

steps <- grab("time-horizon.steps")

message("steps                     [-] : ", steps)
message()

voltage                   <- grab("cm-electricity-1.voltage")

capacity                  <- transgrab("capacity")
resistance.per.metre      <- transgrab("resistance-per-metre")
reactance.per.metre       <- transgrab("reactance-per-metre")
length                    <- transgrab("length")
voltage.angle.delta.upper <- transgrab("voltage-angle-delta-upper")
discretization.steps      <- transgrab("discretization-steps")

message("voltage                   [V] : ", voltage)
message("capacity                  [W] : ", capacity)
message("resistance/metre      [ohm/m] : ", resistance.per.metre)
message("reactance/metre       [ohm/m] : ", reactance.per.metre)
message("length                    [m] : ", length)
message("discretization up       [deg] : ", voltage.angle.delta.upper)
message("discretization steps      [-] : ", discretization.steps)
message()

message("---")
message()

voltage.angle.delta.upper.rad <- deg2rad(voltage.angle.delta.upper)
interval.deg                  <- voltage.angle.delta.upper / discretization.steps
interval.rad                  <- deg2rad(interval.deg)

message("interval                [deg] : ", interval.deg)
message("interval                [rad] : ", interval.rad)
message("discretization cap      [rad] : ", voltage.angle.delta.upper.rad)
message()

resistance <- resistance.per.metre * length
reactance  <- reactance.per.metre  * length

conductance <- calc(+resistance, reactance)
susceptance <- calc(-reactance, resistance)

message("resistance  (R)         [ohm] : ", resistance)
message("reactance   (X)         [ohm] : ", reactance)

message("conductance (G)           [S] : ", conductance)
message("susceptance (B)           [S] : ", susceptance)

V2B   <- voltage ^ 2 * susceptance
V2Gi1 <- voltage ^ 2 * conductance * (2 * 1 - 1) * interval.rad
V2Gi2 <- voltage ^ 2 * conductance * (2 * 2 - 1) * interval.rad
V2Gi3 <- voltage ^ 2 * conductance * (2 * 3 - 1) * interval.rad

message()

message("V2B                       [W] : ", sprintf(fmt, V2B))
message("V2G (1)                   [W] : ", sprintf(fmt, V2Gi1))
if ( discretization.steps > 1 ) message("V2G (2)                   [W] : ", sprintf(fmt, V2Gi2))
if ( discretization.steps > 2 ) message("V2G (3)                   [W] : ", sprintf(fmt, V2Gi3))

message()

# ---------------------------------
#  active code 2
# ---------------------------------

calcFlow <- function(theta.deg,
                     injection)
{
  theta.rad    <- deg2rad(theta.deg)

  message("delta theta (xeona)     [deg] : ", theta.deg)
  message("delta theta             [rad] : ", theta.rad)
  message()

  transmission <- (voltage ^ 2) * +susceptance * (theta.rad)     # B
  lineloss     <- (voltage ^ 2) * +conductance * (theta.rad ^ 2) # G
  exit         <- transmission - lineloss

  relduty.pc   <- (transmission/capacity) * 100
  relloss.pc   <- (lineloss/transmission) * 100

  fmt2 <- "%+.2f"

  scale <- 1.0e+06

  message("injection (xeona)         [W] : ", sprintf(fmt2, injection    / scale))
  message()

  message("transmission (formula)    [W] : ", sprintf(fmt2, transmission / scale))
  message("line loss                 [W] : ", sprintf(fmt2, lineloss     / scale))
  message("exit                      [W] : ", sprintf(fmt2, exit         / scale))
  message()

  message("relative duty             [%] : ", sprintf("%.2f", relduty.pc))
  message("relative loss             [%] : ", sprintf("%.2f", relloss.pc))
  message()
}

message("---")
message()

# specific out-data (in-data automatically read from unrun file)

theta.deg <- -2.661                          # [deg]
injection <- 30.0e+06                        # [W]

# r9047, experimental
# theta.deg <- -7.591                          # [deg]
# injection <- 0.0e+06                         # [W]

calcFlow(theta.deg, injection)

# ---------------------------------
#  housekeeping
# ---------------------------------

quit(save = "no")

#  $Source$
#  end of file

