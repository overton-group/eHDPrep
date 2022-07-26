
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eHDPrep

<!-- badges: start -->
<!-- badges: end -->

The goal of eHDPrep is to provide robust quality control and semantic
enrichment tools for preparation of health datasets. High-level and
low-level functionality are included for general and specialist R users,
respectively. A detailed vignette can be accessed using:
`vignette("Introduction_to_eHDPrep", package = "eHDPrep")`.

## Installation

You can install the released version of eHDPrep from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("eHDPrep")
```

Or from [GitHub](https://github.com) with:

``` r
install.packages("devtools")
devtools::install_github("overton-group/eHDPrep")
```

## Example

eHDPrep can prepare health data for analysis with several approaches.
For example, standardising strings representing missing values to `NA`:

``` r
library(eHDPrep)
data("example_data")

# original values in t_stage variable
unique(example_data$t_stage)
#> [1] "T3a"       "T3b"       "T1"        "T4"        "T2"        "equivocal"

# predefined value "equivocal" is removed
unique(eHDPrep::strings_to_NA(example_data, strings_to_replace = "equivocal")$t_stage)
#> [1] "T3a" "T3b" "T1"  "T4"  "T2"  NA
```

## System Requirements

Some additional system dependencies are required for eHDPrep. These are
detailed below:

### Ubuntu

``` r
remotes::system_requirements(".", os = "ubuntu", os_release = "18.04")
#>  [1] "apt-get install -y libicu-dev"          
#>  [2] "apt-get install -y pandoc"              
#>  [3] "apt-get install -y make"                
#>  [4] "apt-get install -y libcurl4-openssl-dev"
#>  [5] "apt-get install -y libssl-dev"          
#>  [6] "apt-get install -y libxml2-dev"         
#>  [7] "apt-get install -y libfontconfig1-dev"  
#>  [8] "apt-get install -y libfreetype6-dev"    
#>  [9] "apt-get install -y libpng-dev"          
#> [10] "apt-get install -y imagemagick"         
#> [11] "apt-get install -y libmagick++-dev"     
#> [12] "apt-get install -y gsfonts"             
#> [13] "apt-get install -y libglpk-dev"         
#> [14] "apt-get install -y libgmp3-dev"
remotes::system_requirements(".", os = "ubuntu", os_release = "20.04")
#>  [1] "apt-get install -y libicu-dev"          
#>  [2] "apt-get install -y pandoc"              
#>  [3] "apt-get install -y make"                
#>  [4] "apt-get install -y libcurl4-openssl-dev"
#>  [5] "apt-get install -y libssl-dev"          
#>  [6] "apt-get install -y libxml2-dev"         
#>  [7] "apt-get install -y libfontconfig1-dev"  
#>  [8] "apt-get install -y libfreetype6-dev"    
#>  [9] "apt-get install -y libpng-dev"          
#> [10] "apt-get install -y imagemagick"         
#> [11] "apt-get install -y libmagick++-dev"     
#> [12] "apt-get install -y gsfonts"             
#> [13] "apt-get install -y libglpk-dev"         
#> [14] "apt-get install -y libgmp3-dev"
```

### Opensuse

``` r
remotes::system_requirements(".", os = "opensuse", os_release = "42.3")
#>  [1] "zypper install -y libicu-devel"         
#>  [2] "zypper install -y pandoc"               
#>  [3] "zypper install -y make"                 
#>  [4] "zypper install -y libcurl-devel"        
#>  [5] "zypper install -y libopenssl-devel"     
#>  [6] "zypper install -y libxml2-devel"        
#>  [7] "zypper install -y fontconfig-devel"     
#>  [8] "zypper install -y freetype2-devel"      
#>  [9] "zypper install -y libpng16-compat-devel"
#> [10] "zypper install -y ImageMagick"          
#> [11] "zypper install -y ImageMagick-devel"    
#> [12] "zypper install -y libMagick++-devel"    
#> [13] "zypper install -y gmp-devel"
```

### CentOS

``` r
remotes::system_requirements(".", os = "centos", os_release = "7")
#>  [1] "yum install -y epel-release"         
#>  [2] "yum install -y libicu-devel"         
#>  [3] "yum install -y pandoc"               
#>  [4] "yum install -y make"                 
#>  [5] "yum install -y libcurl-devel"        
#>  [6] "yum install -y openssl-devel"        
#>  [7] "yum install -y libxml2-devel"        
#>  [8] "yum install -y fontconfig-devel"     
#>  [9] "yum install -y freetype-devel"       
#> [10] "yum install -y libpng-devel"         
#> [11] "yum install -y ImageMagick"          
#> [12] "yum install -y ImageMagick-c++-devel"
#> [13] "yum install -y glpk-devel"           
#> [14] "yum install -y gmp-devel"
remotes::system_requirements(".", os = "centos", os_release = "8")
#>  [1] "dnf install -y dnf-plugins-core"            
#>  [2] "dnf config-manager --set-enabled powertools"
#>  [3] "dnf install -y epel-release"                
#>  [4] "dnf install -y libicu-devel"                
#>  [5] "dnf install -y pandoc"                      
#>  [6] "dnf install -y make"                        
#>  [7] "dnf install -y libcurl-devel"               
#>  [8] "dnf install -y openssl-devel"               
#>  [9] "dnf install -y libxml2-devel"               
#> [10] "dnf install -y fontconfig-devel"            
#> [11] "dnf install -y freetype-devel"              
#> [12] "dnf install -y libpng-devel"                
#> [13] "dnf install -y ImageMagick"                 
#> [14] "dnf install -y ImageMagick-c++-devel"       
#> [15] "dnf install -y glpk-devel"                  
#> [16] "dnf install -y gmp-devel"
```

### Red Hat

``` r
remotes::system_requirements(".", os = "redhat", os_release = "7")
#>  [1] "rpm -q epel-release || yum install -y https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm"
#>  [2] "yum install -y libicu-devel"                                                                                 
#>  [3] "yum install -y pandoc"                                                                                       
#>  [4] "yum install -y make"                                                                                         
#>  [5] "yum install -y libcurl-devel"                                                                                
#>  [6] "yum install -y openssl-devel"                                                                                
#>  [7] "yum install -y libxml2-devel"                                                                                
#>  [8] "yum install -y fontconfig-devel"                                                                             
#>  [9] "yum install -y freetype-devel"                                                                               
#> [10] "yum install -y libpng-devel"                                                                                 
#> [11] "yum install -y ImageMagick"                                                                                  
#> [12] "yum install -y ImageMagick-c++"                                                                              
#> [13] "yum install -y glpk-devel"                                                                                   
#> [14] "yum install -y gmp-devel"
remotes::system_requirements(".", os = "redhat", os_release = "8")
#>  [1] "dnf install -y https://dl.fedoraproject.org/pub/epel/epel-release-latest-8.noarch.rpm"
#>  [2] "dnf install -y libicu-devel"                                                          
#>  [3] "dnf install -y make"                                                                  
#>  [4] "dnf install -y libcurl-devel"                                                         
#>  [5] "dnf install -y openssl-devel"                                                         
#>  [6] "dnf install -y libxml2-devel"                                                         
#>  [7] "dnf install -y fontconfig-devel"                                                      
#>  [8] "dnf install -y freetype-devel"                                                        
#>  [9] "dnf install -y libpng-devel"                                                          
#> [10] "dnf install -y ImageMagick"                                                           
#> [11] "dnf install -y ImageMagick-c++"                                                       
#> [12] "dnf install -y glpk-devel"                                                            
#> [13] "dnf install -y gmp-devel"
```
