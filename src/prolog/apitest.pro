:- use_module(library(http/http_client)).


apitest(ReturnData) :-
    Key = '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',
    http_post(
        'https://api.openai.com/v1/chat/completions',
        atom(application/json, '{"model": "gpt-3.5-turbo","messages": [{"role": "user", "content": "Say this is a test!"}],"temperature": 0.7}'),
        ReturnData,
        [authorization(bearer(Key)),application/json]).


