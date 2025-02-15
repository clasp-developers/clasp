#include <chrono>
#include <stack>
#include <string>
#include <unordered_map>
#include <mutex>
#include <vector>
#include <memory>
#include <sstream>
#include <locale>
#include <iomanip>
#include <functional> // For std::hash
#include <cmath>      // For HSV to RGB conversion
#include <cstdio>     // For sscanf

class Profiler {
public:
    // Start timing a code region with a given name
    static void RangePush(const std::string& name) {
        // Record the current time
        auto now = std::chrono::high_resolution_clock::now();

        // If this is the first call, initialize the root region
        if (!rootRegion) {
            rootRegion = std::make_shared<RegionNode>("Root");
            rootRegion->startTime = now; // Record the start time of the root
        }

        RegionNode* current;
        if (regionStack.empty()) {
            // If the stack is empty, start from the root region
            current = rootRegion.get();
        } else {
            // Otherwise, get the current region from the stack
            current = regionStack.top();
        }

        // Check if the child region already exists
        auto it = current->children.find(name);
        if (it != current->children.end()) {
            // Child exists; reuse it
            regionStack.push(it->second.get());
        } else {
            // Create a new child region
            auto newRegion = std::make_shared<RegionNode>(name, current);
            newRegion->startTime = now; // Record the start time of the new region
            current->children[name] = newRegion;
            regionStack.push(newRegion.get());
        }

        // Push the current time onto the timing stack
        getTimingStack().emplace(now);
    }

    // End timing the most recent code region
    static void RangePop() {
        // Record the current time
        auto now = std::chrono::high_resolution_clock::now();

        // Check for matching RangePush calls
        if (getTimingStack().empty() || regionStack.empty()) {
            return; // No matching RangePush; do nothing
        }

        // Get the start time from the timing stack
        auto start_time = getTimingStack().top();
        getTimingStack().pop();

        // Get the current region from the region stack
        RegionNode* current = regionStack.top();
        regionStack.pop();

        // Calculate the duration in microseconds
        auto duration = std::chrono::duration_cast<std::chrono::microseconds>(now - start_time).count();
        current->totalTime += duration;

        // If the region stack is empty, we've returned to the root
        if (regionStack.empty()) {
            // Calculate the root node's total time
            auto rootDuration = std::chrono::duration_cast<std::chrono::microseconds>(now - rootRegion->startTime).count();
            rootRegion->totalTime = rootDuration;

            // Store the root region for reporting
            std::lock_guard<std::mutex> lock(dataMutex);
            allThreadRegions.push_back(rootRegion);
            rootRegion.reset();
        }
    }

    // Report the accumulated timing data to the provided output stream
    static void Report(std::ostream& outStream) {
        std::lock_guard<std::mutex> lock(dataMutex);
        // Print each root region
        for (const auto& root : allThreadRegions) {
            printRegion(outStream, root.get(), 0);
        }
    }

    // Generate an HTML string containing the flame graph
    static std::string ReportFlameGraph() {
        std::lock_guard<std::mutex> lock(dataMutex);

        if (allThreadRegions.empty()) {
            return "<html><body><p>No profiling data available.</p></body></html>";
        }

        // Find the maximum root time among all root regions
        long long maxRootTime = 0;
        for (const auto& root : allThreadRegions) {
            if (root->totalTime > maxRootTime) {
                maxRootTime = root->totalTime;
            }
        }

        // Set the SVG width and calculate the unit width based on the maximum root time
        double svgWidth = 1000.0;
        double unitWidth = svgWidth / maxRootTime;

        // SVG content elements
        std::vector<std::string> svgElements;

        // Vector to hold clipping paths
        std::vector<std::string> clipPaths;

        // Vertical positioning variables
        int currentDepthOffset = 0;
        int verticalSpacing = 20; // Space between flame graphs

        // Calculate the total SVG height
        for (const auto& root : allThreadRegions) {
            int depth = getDepth(root.get());
            currentDepthOffset += (depth + 1) * 20 + verticalSpacing;
        }

        // Reset the current depth offset for rendering
        currentDepthOffset = 0;

        // Static counter for generating unique IDs
        int clipPathIdCounter = 0;

        // Process each root region to generate SVG elements
        for (const auto& root : allThreadRegions) {
            // Starting X position
            double currentX = 0.0;

            // Process the root region and its children
            processRegion(root.get(), currentX, unitWidth, 0, svgElements, currentDepthOffset, clipPaths, clipPathIdCounter);

            // Increment the current depth offset for the next root region
            int depth = getDepth(root.get());
            currentDepthOffset += (depth + 1) * 20 + verticalSpacing;
        }

        // Assemble the SVG content
        std::ostringstream svgContent;
        svgContent << "<svg width=\"" << svgWidth << "\" height=\"" << currentDepthOffset << "\" "
                      "xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">";
        svgContent << "<style>text { pointer-events: none; font-family: Arial, sans-serif; }</style>";

        // Add clipping paths inside <defs>
        if (!clipPaths.empty()) {
            svgContent << "<defs>";
            for (const auto& clipPath : clipPaths) {
                svgContent << clipPath << "\n";
            }
            svgContent << "</defs>";
        }

        // Add all SVG elements
        for (const auto& element : svgElements) {
            svgContent << element << "\n";
        }

        svgContent << "</svg>";

        // Embed the SVG in HTML with UTF-8 encoding
        std::ostringstream htmlContent;
        htmlContent << "<html><head><meta charset=\"UTF-8\"></head><body>\n"
                    << svgContent.str()
                    << "\n</body></html>";

        return htmlContent.str();
    }

    // Reset the Profiler's data structures
    static void Reset() {
        std::lock_guard<std::mutex> lock(dataMutex);
        allThreadRegions.clear();
        rootRegion.reset();
        while (!regionStack.empty()) {
            regionStack.pop();
        }
    }

private:
    using TimePoint = std::chrono::high_resolution_clock::time_point;

    // Structure to represent a profiling region
    struct RegionNode {
        std::string name; // Name of the region
        long long totalTime = 0; // Total time spent in the region (microseconds)
        std::unordered_map<std::string, std::shared_ptr<RegionNode>> children; // Child regions
        RegionNode* parent = nullptr; // Parent region
        TimePoint startTime; // Start time of the region

        RegionNode(const std::string& name, RegionNode* parent = nullptr)
            : name(name), parent(parent) {}
    };

    // Get the thread-local timing stack
    static std::stack<TimePoint>& getTimingStack() {
        static thread_local std::stack<TimePoint> timingStack;
        return timingStack;
    }

    // Helper function to print the region tree recursively to the provided output stream
    static void printRegion(std::ostream& outStream, RegionNode* node, int indent) {
        // Indent the output based on the depth
        for (int i = 0; i < indent; ++i) {
            outStream << "  ";
        }
        // Print the region name and total time
        outStream << node->name << ": " << node->totalTime << " microseconds\n";
        // Recursively print child regions
        for (const auto& child : node->children) {
            printRegion(outStream, child.second.get(), indent + 1);
        }
    }

    // Helper function to generate a color string from a node name using HSV
    static std::string getColorForName(const std::string& name) {
        // Use std::hash to get a hash value of the name
        std::size_t hashValue = std::hash<std::string>{}(name);

        // Map the hash value to a hue between 0 and 360 degrees
        double hue = static_cast<double>(hashValue % 360);

        // Set saturation and value to 0.7 for bright colors (range is 0 to 1)
        double saturation = 0.7;
        double value = 0.9;

        // Convert HSV to RGB
        double c = value * saturation;
        double x = c * (1 - std::fabs(fmod(hue / 60.0, 2) - 1));
        double m = value - c;

        double r_prime, g_prime, b_prime;

        if (hue >= 0 && hue < 60) {
            r_prime = c;
            g_prime = x;
            b_prime = 0;
        } else if (hue >= 60 && hue < 120) {
            r_prime = x;
            g_prime = c;
            b_prime = 0;
        } else if (hue >= 120 && hue < 180) {
            r_prime = 0;
            g_prime = c;
            b_prime = x;
        } else if (hue >= 180 && hue < 240) {
            r_prime = 0;
            g_prime = x;
            b_prime = c;
        } else if (hue >= 240 && hue < 300) {
            r_prime = x;
            g_prime = 0;
            b_prime = c;
        } else { // hue >= 300 && hue < 360
            r_prime = c;
            g_prime = 0;
            b_prime = x;
        }

        int r = static_cast<int>((r_prime + m) * 255);
        int g = static_cast<int>((g_prime + m) * 255);
        int b = static_cast<int>((b_prime + m) * 255);

        // Create the color string in RGB format
        std::ostringstream colorStream;
        colorStream << "rgb(" << r << "," << g << "," << b << ")";
        return colorStream.str();
    }

    // Helper function to determine the appropriate text color (black or white) based on background brightness
    static std::string getTextColor(int r, int g, int b) {
        // Calculate the luminance
        double luminance = (0.299 * r + 0.587 * g + 0.114 * b) / 255;

        // If luminance is high, use black text; otherwise, use white text
        return (luminance > 0.5) ? "black" : "white";
    }

    // Helper function to process each region and generate SVG elements for the flame graph
    static void processRegion(
        RegionNode* node,
        double rectX,
        double unitWidth,
        int depth,
        std::vector<std::string>& svgElements,
        int currentDepthOffset,
        std::vector<std::string>& clipPaths,
        int& clipPathIdCounter,
        long long parentTotalTime = 0) {

        // Calculate the rectangle's position and size
        double rectY = currentDepthOffset + depth * 20;
        double rectHeight = 20;
        double rectWidth = node->totalTime * unitWidth;

        // Calculate the percentage of the parent's time (if applicable)
        double percentage = 100.0;
        if (parentTotalTime > 0) {
            percentage = (static_cast<double>(node->totalTime) / parentTotalTime) * 100.0;
        }

        // Format the total time with commas
        std::ostringstream timeStream;
        timeStream.imbue(std::locale(""));
        timeStream << std::fixed << node->totalTime;

        // Format the percentage
        std::ostringstream percentStream;
        percentStream << std::fixed << std::setprecision(2) << percentage << "%";

        // Generate the tooltip content
        std::ostringstream tooltipStream;
        tooltipStream << node->name << "\n"
                      << "Time: " << timeStream.str() << " &#956;s\n";
        if (parentTotalTime > 0) {
            tooltipStream << "Parent Time: " << percentStream.str();
        }

        // Generate a unique ID for the clipping path
        int currentClipPathId = clipPathIdCounter++;
        std::string clipPathId = "clip" + std::to_string(currentClipPathId);

        // Define the clipping path
        std::ostringstream clipPathElement;
        clipPathElement << "<clipPath id=\"" << clipPathId << "\">"
                        << "<rect x=\"" << rectX << "\" y=\"" << rectY
                        << "\" width=\"" << rectWidth << "\" height=\"" << rectHeight << "\" />"
                        << "</clipPath>";

        // Add the clipping path to the collection
        clipPaths.push_back(clipPathElement.str());

        // Get the color for this node
        std::string fillColor = getColorForName(node->name);

        // Extract RGB components from the fill color for text color calculation
        int r, g, b;
        sscanf(fillColor.c_str(), "rgb(%d,%d,%d)", &r, &g, &b);

        // Determine the text color based on background brightness
        std::string textColor = getTextColor(r, g, b);

        // Begin the SVG group element
        std::ostringstream groupElement;
        groupElement << "<g>";

        // Add the tooltip using the <title> element
        groupElement << "<title>" << tooltipStream.str() << "</title>";

        // Generate the rectangle SVG element with the color
        std::ostringstream rectElement;
        rectElement << "<rect x=\"" << rectX << "\" y=\"" << rectY
                    << "\" width=\"" << rectWidth << "\" height=\"" << rectHeight
                    << "\" style=\"fill:" << fillColor << ";stroke:black;stroke-width:1\" />";

        // Generate the text SVG element with clipping and text color
        std::ostringstream textElement;
        textElement << "<text x=\"" << rectX + 2 << "\" y=\"" << rectY + 15
                    << "\" font-size=\"12\" fill=\"" << textColor
                    << "\" clip-path=\"url(#" << clipPathId << ")\">"
                    << node->name << " (" << timeStream.str() << " &#956;s)"
                    << "</text>";

        // Close the SVG group element
        groupElement << rectElement.str() << textElement.str() << "</g>";

        // Add the group element to the SVG elements list
        svgElements.push_back(groupElement.str());

        // If the node has no time or width, we cannot render its children properly
        if (node->totalTime <= 0 || rectWidth <= 0) {
            return;
        }

        // Process child regions recursively
        double childX = rectX;
        for (const auto& child : node->children) {
            // Process the child region
            processRegion(
                child.second.get(),
                childX,
                unitWidth,
                depth + 1,
                svgElements,
                currentDepthOffset,
                clipPaths,
                clipPathIdCounter,
                node->totalTime); // Pass the current node's total time as the parent's total time
            // Update the X position for the next sibling
            double childWidth = child.second->totalTime * unitWidth;
            childX += childWidth;
        }
    }

    // Helper function to get the maximum depth of the region tree
    static int getDepth(RegionNode* node) {
        int maxChildDepth = 0;
        // Recursively determine the depth of child regions
        for (const auto& child : node->children) {
            int childDepth = getDepth(child.second.get());
            if (childDepth > maxChildDepth) {
                maxChildDepth = childDepth;
            }
        }
        // Return the depth of the current node plus the maximum depth of its children
        return 1 + maxChildDepth;
    }

    // Thread-local variables
    static thread_local std::shared_ptr<RegionNode> rootRegion;
    static thread_local std::stack<RegionNode*> regionStack;

    // Shared data structures
    static std::vector<std::shared_ptr<RegionNode>> allThreadRegions;
    static std::mutex dataMutex;
};

// Static member definitions
thread_local std::shared_ptr<Profiler::RegionNode> Profiler::rootRegion = nullptr;
thread_local std::stack<Profiler::RegionNode*> Profiler::regionStack;
std::vector<std::shared_ptr<Profiler::RegionNode>> Profiler::allThreadRegions;
std::mutex Profiler::dataMutex;
