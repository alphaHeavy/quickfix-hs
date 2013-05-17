#include <quickfix/Application.h>
#include <HsFFI.h>

extern "C"
{
    FIX::Application* newApplication(HsPtr app);
    void deleteApplication(FIX::Application* app);
}

