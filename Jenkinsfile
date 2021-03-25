#!groovy

  pipeline {
    agent {
      label 'r-slave'
    }
    stages{
      stage('Build - json2test') {
        steps {
          echo 'building json2test'
          sh """
            #!/bin/bash
            sudo chmod 777 ~/workspace
            cd ~/workspace
            if [ -d "json2test" ]; then
              sudo rm -R json2test
            fi
            git clone git@github.com:InsightRX/json2test.git
            cd json2test
            git checkout $GIT_BRANCH
            sudo chmod +x slack_notification.sh
            sudo Rscript -e "install.packages(c('tibble', 'testthat'), lib='/usr/lib/R/site-library', repos='https://cran.rstudio.com', dependencies = TRUE)"
            R CMD INSTALL . --library=/usr/lib/R/site-library || { export STATUS=failed
            ./slack_notification.sh
            exit 1
            }
            R CMD check . --no-manual
          """
        }
      }
    }
  }
