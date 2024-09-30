
import subprocess
import sys
import os

os.environ['NO_PROXY'] = ''
os.environ['HTTP_PROXY'] = 'http://proxy-rie.http.insee.fr:8080'
os.environ['HTTPS_PROXY'] = 'http://proxy-rie.http.insee.fr:8080'

def install(package):
    subprocess.check_call([sys.executable, "-m", "pip", "install", package])

# Liste des packages à installer
packages = ['setuptools', 'libpff-python']

# Installation des packages
for package in packages:
    try:
        print(f"Installation de {package}...")
        install(package)
        print(f"{package} installé avec succès.")
    except subprocess.CalledProcessError:
        print(f"Erreur lors de l'installation de {package}")
