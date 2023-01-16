library(openai)
library(dplyr)
library(tm)

# Load the OpenAI API key
openai_api_key <- "XX-XXXXXXXXXXXXXXXXXXXXXXXXXXX"

# Get a list of available models
modelList <- openai::list_models(openai_api_key = openai_api_key)
modelList <- modelList$data$id

# Define a function to test each model with a given prompt
testEachModel <- function(x, prompt){
  # Set the user prompt
  userPrompt <- prompt
  
  # Use a try-catch block to handle potential errors
  tryCatch({
    # Use the OpenAI API to generate a completion for the prompt
    response <- openai::create_completion(prompt = userPrompt, 
                                          model = x, 
                                          openai_api_key = openai_api_key,
                                          max_tokens = 1000,
                                          temperature = .1)
    # Create a data frame to store the prompt and response
    df <- data.frame(prompt = userPrompt,
                     model_response = stringr::str_squish(gsub("\\n|,|'|]", "", response$choices$text)))
    # Add the model name to the data frame
    df <- df %>% mutate(model = x)
    # Print the data frame
    print(df)
    # Sleep for 2 seconds to avoid rate limiting
    Sys.sleep(2)
    # Return the data frame
    return(df)
  }, error = function(e) {
    # If an error occurs, create a data frame with an "ERROR" message
    df <- data.frame(prompt = userPrompt,
                     model_response = "ERROR",
                     model = x)
    return(df)
  })
}

# Test each model with the prompt "what is temperature in natural language processing?"
eachModelResponse_raw <- lapply(modelList, testEachModel, "what is temperature in natural language processing?")
# Combine the results into a single data frame
eachModelResponse <- do.call(rbind.data.frame, eachModelResponse_raw, quote = FALSE)

# Clean up the model_response column
eachModelResponse <- eachModelResponse %>% mutate(model_response = stringr::str_squish(model_response),
                                                  n_chars = nchar(model_response))

