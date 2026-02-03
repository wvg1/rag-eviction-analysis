"""
This code sample shows Prebuilt Read operations with the Azure AI Document Intelligence client library.
The async versions of the samples require Python 3.8 or later.

To learn more, please visit the documentation - Quickstart: Document Intelligence (formerly Form Recognizer) SDKs
https://learn.microsoft.com/azure/ai-services/document-intelligence/quickstarts/get-started-sdks-rest-api?pivots=programming-language-python
"""

from azure.core.credentials import AzureKeyCredential
from azure.ai.documentintelligence import DocumentIntelligenceClient
from azure.ai.documentintelligence.models import AnalyzeDocumentRequest
import numpy as np
import base64
import os

"""
Remember to remove the key from your code when you're done, and never post it publicly. For production, use
secure methods to store and access your credentials. For more information, see 
https://docs.microsoft.com/en-us/azure/cognitive-services/cognitive-services-security?tabs=command-line%2Ccsharp#environment-variables-and-application-configuration
"""
endpoint = "https://pilotpilotpilot.cognitiveservices.azure.com/"
key = "4ac14efedf044fd1a70311208d354f1a"

def analyze_layout_custom_model(file_path, model_id = "minute-entry-caseno-rep-hearingdate-final"):     
    with open(file_path, "rb") as f:
        base64_encoded_pdf = base64.b64encode(f.read()).decode("utf-8")

    analyze_request = {
        "base64Source": base64_encoded_pdf
    }

    document_intelligence_client = DocumentIntelligenceClient(
        endpoint=endpoint, credential=AzureKeyCredential(key)
    )

    poller = document_intelligence_client.begin_analyze_document(
        model_id = model_id, body = analyze_request
    )

    result = poller.result()

    # if result.styles and any([style.is_handwritten for style in result.styles]):
    #     print("Document contains handwritten content")
    # else:
    #     print("Document does not contain handwritten content")

    # for page in result.pages:
    #     print(f"----Analyzing layout from page #{page.page_number}----")
    #     print(
    #         f"Page has width: {page.width} and height: {page.height}, measured with unit: {page.unit}"
    #     )

    #     if page.lines:
    #         for line in page.lines:
    #             print(f"Content: {line.content}")  # Print content of the line

    return result