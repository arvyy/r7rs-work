pipeline {
    
    agent {
        label 'docker'
    }

    stages {

        stage('Checkout') {
            steps {
                git changelog: true, branch: "${BRANCH_NAME}", url: 'https://github.com/arvyy/r7rs-work'
            }
        }

        stage('Test DateTime') {
            agent {
                docker {
                    image 'schemers/gauche:head'
                }
            }
            steps {
                sh '''
                    cd DateTime;
                    gosh -I . date-time-test.scm
                '''
            }
        }

    }

}
