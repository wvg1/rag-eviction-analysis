@"
import os
import base64
from openai import AzureOpenAI
from dotenv import load_dotenv

# load environment variables
load_dotenv()

def truncate_text(text, max_chars=200000):
    """
    checks if text is too long and truncate if so
    
    text: the string to check
    max_chars: maximum allowed characters
    
    """
    if len(text) <= max_chars:
        
        return text, False
    
    truncated = text[:max_chars] 
    
    # try to cut at a sentence end (find last period)
    last_period = truncated.rfind('.') 
    
    if last_period > max_chars * 0.95:  
        truncated = truncated[:last_period + 1]  
    
    return truncated, True  

def data_from_llm(system_prompt, full_text):
    
    #truncate if needed
    processed_text, was_truncated = truncate_text(full_text, max_chars=200000)

    if was_truncated:
        print(f"Warning: Truncated from {len(full_text):,} to {len(processed_text):,} chars")
    
    # initialize Azure OpenAI client
    client = AzureOpenAI(
        azure_endpoint=os.getenv("AZURE_OPENAI_ENDPOINT"),
        api_key=os.getenv("AZURE_OPENAI_KEY"),
        api_version="2025-01-01-preview",
    )

    # prepare the chat prompt
    chat_prompt = [
        {
            "role": "system",
            "content": [
                {
                    "type": "text",
                    "text": system_prompt}
            ]
        },
        {
            "role": "user",
            "content": [
                {
                    "type": "text",
                    "text": processed_text}
            ]
        }
    ]

    # generate the completion
    completion = client.chat.completions.create(
        model=os.getenv("AZURE_OPENAI_DEPLOYMENT"),
        messages=chat_prompt,
        max_tokens=2000,
        temperature=0.7,
        top_p=0.95,
        frequency_penalty=0,
        presence_penalty=0,
        stop=None,
        stream=False, 
        logprobs=True
    )
    
    return completion.to_json()
"@ | Out-File -Encoding UTF8 scripts/data_from_llm.py