# pip install libpff-python
import pypff
from pprint import pprint
from tqdm import tqdm
import uuid
import re

def read_pst(file_path):
    pst_file = pypff.file()
    pst_file.open(file_path)

    root_folder = pst_file.get_root_folder()
    process_folder(root_folder)

    pst_file.close()


def process_folder(folder, liste_message_structure=None, message_count=0, total_messages=None):
    if liste_message_structure is None:
        liste_message_structure = []
    
    if total_messages is None:
        total_messages = count_messages(folder)
        pbar = tqdm(total=total_messages, desc="Processing messages")
    else:
        pbar = None

    for sub_folder in folder.sub_folders:
        message_count, liste_message_structure = process_folder(sub_folder, liste_message_structure, message_count, total_messages)
    
    for message in folder.sub_messages:
        message_count += 1
        if message_count % 500 == 0:
            print(f"Processed {message_count} messages")
        
        liste_message_structure.append(extract_message_info(message))
        
        if pbar:
            pbar.update(1)
    
    if pbar:
        pbar.close()
    
    return message_count, liste_message_structure

def count_messages(folder):
    count = len(folder.sub_messages)
    for sub_folder in folder.sub_folders:
        count += count_messages(sub_folder)
    return count

def extract_message_info(message, parent_id=None):
    transport_headers = getattr(message, 'transport_headers', None)
    if transport_headers is None:
        transport_headers = getattr(message, 'get_transport_headers', lambda: 'N/A')()
    
    if not isinstance(transport_headers, str):
        transport_headers = str(transport_headers)
    
    # Extraction des destinataires
    to_field = re.search(r'To:(.+?)(?:\n\S|\Z)', transport_headers, re.DOTALL)
    recipients = []
    if to_field:
        to_content = to_field.group(1)
        recipients = re.findall(r'[\w\.-]+@[\w\.-]+', to_content)
    
    # Extraction de l'expéditeur
    from_field = re.search(r'From:(.+?)(?:\n\S|\Z)', transport_headers, re.DOTALL)
    sender = ''
    if from_field:
        from_content = from_field.group(1)
        sender_match = re.search(r'[\w\.-]+@[\w\.-]+', from_content)
        if sender_match:
            sender = sender_match.group(0)
    
    # Extraction de la date
    date_field = re.search(r'Date:(.+?)(?:\n\S|\Z)', transport_headers, re.DOTALL)
    date = date_field.group(1).strip() if date_field else 'N/A'
    
    # Extraction du sujet
    subject_field = re.search(r'Subject:(.+?)(?:\n\S|\Z)', transport_headers, re.DOTALL)
    subject = subject_field.group(1).strip() if subject_field else 'N/A'

    # Extraction du corps du message
    body = None
    try:
        body = message.plain_text_body
    except OSError:
        pass

    if body is None:
        try:
            body = message.html_body
        except OSError:
            pass

    if body is None:
        body = "N/A"
    else:
        try:
            body = body.decode('utf-8', errors='ignore')
        except AttributeError:
            body = str(body)

    # Générer un ID unique pour ce message
    message_id = str(uuid.uuid4())

    # Extraire les messages précédents
    previous_messages = extract_previous_messages(body)

    objet = {
        "id": message_id,
        "parent_id": parent_id,
        "sender": sender,
        "recipients": recipients,
        "date": date,
        "subject": subject,
        "body": body,
        "previous_messages": previous_messages
    }
    
    pprint(objet, width=80, sort_dicts=False)

    return objet

def extract_previous_messages(body):
    # Regex pour trouver les en-têtes des messages précédents
    header_pattern = re.compile(r'De\s*:\s*(.+?)\r?\nEnvoyé\s*:\s*(.+?)\r?\nÀ\s*:\s*(.+?)\r?\nObjet\s*:\s*(.+?)\r?\n', re.DOTALL)
    
    messages = []
    current_position = 0

    while True:
        match = header_pattern.search(body, current_position)
        if not match:
            break

        start = match.start()
        next_match = header_pattern.search(body, match.end())
        end = next_match.start() if next_match else len(body)

        message_body = body[match.end():end].strip()

        message = {
            "id": str(uuid.uuid4()),
            "sender": match.group(1).strip(),
            "date": match.group(2).strip(),
            "recipients": match.group(3).strip(),
            "subject": match.group(4).strip(),
            "body": message_body
        }

        messages.append(message)
        current_position = end

    return messages

file_path = "backup.pst"
pst_file = pypff.file()
pst_file.open(file_path)
root_folder = pst_file.get_root_folder()
process_folder(root_folder)


# Idées :
- dico structuré, save en §JSON pour lecture sur R après
- nombre de mail par jour
- tableau de bord des heures du nombre envois de mail par personne
- graph entre les individus qui répondent etc..et se renvoit
- nombre de mots par mail, nombre de mots uniques utilisés etc..
- liens pondérés par nb mails envoyés, couleurs des noeuds dépendant de la div
- faire des records
- mail le plus long
 #C:/Users/RK09OA/AppData/Local/Programs/Python/Python312/python.exe , pour lancer python sur terminal





