# Customising and Distributing the AAGI Collaborator Survey

## Introduction

Every experimental design or data analysis output delivered as part of a
Service & Support collaboration should be accompanied by an opportunity
for collaborators to provide feedback via the AAGI Collaborator Survey.
To ensure only relevant questions are shown and that key contextual
information is captured alongside responses, each survey link must be
customised before sharing - this means selecting details specific to the
output and recipient, such as the type of support provided, the AAGI
node, and the collaborator’s organisation type, which are then encoded
directly into the link.

The
[`create_survey_url()`](https://AAGI-AUS.github.io/AAGISurvey/reference/create_survey_url.md)
function makes this straightforward. Run it once per output delivery,
and it will generate the correct link, print a summary of your
selections for you to verify, and copy the URL to your clipboard ready
to paste into your delivery email.

## Installation and Use

### Step 1: Enable the AAGI R-Universe

To install the {AAGISurvey} package containing the
[`create_survey_url()`](https://AAGI-AUS.github.io/AAGISurvey/reference/create_survey_url.md)
function, you first need to enable the AAGI R Universe in your R
session. You only need to do this once. If you have already enabled the
AAGI R Universe for another package, you can skip steps 1 & 2 and jump
straight to [Step 3. Load the
package](https://AAGI-AUS.github.io/AAGISurvey/articles/link_generation_guide.qmd#Step%203:%20Load%20the%20package).

``` r
# example syntax for enabling the AAGI R-Universe
options(
  repos = c(
    AAGI = "https://aagi-aus.r-universe.dev",
    CRAN = "https://cloud.r-project.org"
  )
)
```

### Step 2: Install the package

``` r
install.packages("AAGISurvey")
```

### Step 3: Load the package

``` r
library(AAGISurvey)
```

Once installed, you only need
[`library(AAGISurvey)`](https://aagi-aus.github.io/AAGISurvey) in future
sessions.

### Step 4. Run the `create_survey_url()` function

The
[`create_survey_url()`](https://AAGI-AUS.github.io/AAGISurvey/reference/create_survey_url.md)
can be run in one of two ways:

- **Scripted** – provide all required arguments directly in your R
  script.
- **Interactive** – run the function with no arguments and follow the
  prompts in the console.

**Scripted example:**

``` r
library(AAGISurvey)

# Create a survey URL for design support for small plot
# trial for a government agency carried out by AAGI Cu

url <- create_survey_url(
  support_type = "S_D",
  design_type = "D_SP",
  analysis_type = ,
  aagi_node = "CU",
  organisation_type = "O_GRO"
)
```

**Interactive example**

``` r
library(AAGISurvey)

url <- create_survey_url()
```

**Both approaches will:**

- Generate a Qualtrics survey URL containing the correct metadata
- Print a summary of your selections
- Copy the URL automatically to your clipboard, ready for easy pasting
  into an email or other message.

Check the printed summary carefully to confirm all details match the
recipient and output.

**Accessing Full Function Documentation**

For detailed information about all function arguments, including valid
coded values for the scripted example, you can view the full
documentation in R:

``` r
?create_survey_url

# or

help("create_survey_url")
```

### Step 5. Include the Customised Survey Link in Your Output Delivery Email

Include the survey link in the same email that delivers your output.
Research consistently shows that survey response rates and data quality
are meaningfully affected by how and when a survey is distributed
([Dillman et al. 2014](#ref-Dillman2014)). Sending the survey alongside
the output rather than in a separate follow-up addresses several of
these factors directly: the email comes from someone the recipient
already has a working relationship with, which research shows
meaningfully increases response rates ([Ericson 2023](#ref-Ericson2023);
[Millar et al. 2021](#ref-Millar2021)); the request is contextually tied
to a specific piece of work rather than arriving as a generic follow-up;
and collaborators can respond while the experience is fresh without
being asked to recall details later. Reducing the number of separate
communications also lowers the friction that is well-documented as a
contributor to survey non-response ([Dillman et al.
2014](#ref-Dillman2014)).

When adding the link to your email, make sure to:

- hyperLink your customised survey URL to the “Go to survey” button.
- Include the full survey URL as plain text below the button so that, if
  the button does not render correctly in the recipient’s email client,
  it can be copied and pasted directly into a browser.

#### Template Text to include in your output delivery email

> **Note**
>
> Feedback from our collaborators helps us identify patterns,
> differences, and emerging insights across the contexts and groups we
> work with - informing how we prioritise and adapt our work over time
> to remain relevant and responsive to your needs.
>
> To make providing feedback easy, this email includes a link to a
> short, anonymous survey about this output and your collaboration
> experience. Completing the survey takes less than five minutes.
>
> ![](images/go_to_survey_button.png)
>
> (Alternatively, copy and paste this URL into your browser:
> http://YOUR_CUSTOMISED_SURVEY_URL.com)
>
> Thank you for taking the time to share your thoughts.

### Step 6. Save a copy of the email for project records

After sending the email, save a copy in the ‘Correspondence’ folder of
the associated collaboration project. This ensures a record is
maintained for future reference and accountability.  

## Further Information

### About the Survey

The AAGI Collaborator Survey is designed to help us better understand
the impact our Service & Support activities are having within the
Australian grains research, development and extension (RD&E) sector. It
targets researchers we work with directly through Service & Support
collaborations, gathering structured feedback on experimental design and
data analysis outputs and the collaboration experience surrounding them.
The goal is not only to understand where and how our support is
effective, but to identify opportunities to strengthen how we work with
collaborators over time.

### About the Survey Design

The survey was designed with data quality as the primary consideration,
though what quality means for this kind of data differs in important
ways from how it is defined for objective or directly measured
quantities, and is worth briefly unpacking.

Surveys collect information through participants’ responses, which means
they capture what people report, recall, perceive, or judge. This
information is inherently shaped by individual experience,
interpretation, and context. This is not a weakness. It is the defining
characteristic of the data surveys are built to capture, and one of the
primary reasons surveys exist: to provide an efficient, scalable way to
access information that cannot be measured directly with instruments.
Things like experiences, perceptions, judgements, and interpretations.
For this kind of data, quality is not defined by statistical precision
or replicability in the instrumental sense. It is defined by how well
responses reflect participants’ genuine views and experiences given the
questions asked and the context in which they were answered.

Understanding this shapes every design decision. Minimising objective
burden (the measurable effort imposed by a survey’s length, structure,
and demands) is a baseline obligation of good research practice. Asking
people to invest time and effort unnecessarily is poor design and, more
fundamentally, poor ethics. But research suggests that it is perceived
burden, rather than objective burden, that is the more decisive
determinant of survey response (data) quality ([Kunz and Gummer
2024](#ref-KunzGummer2024)). Perceived burden reflects each respondent’s
subjective experience of the survey, how difficult, effortful, or
worthwhile they find it, and consistently predicts response quality
across a range of indicators including item completion, attention, and
response depth ([Kunz and Gummer 2024](#ref-KunzGummer2024)).

Perceived burden is not determined by effort alone. It reflects the
relationship between perceived effort and perceived value, and research
consistently identifies motivation, trust, and topic salience as
interacting factors that shape whether people participate, complete a
survey, and invest in the quality of their responses ([Krosnick
1991](#ref-Krosnick1991); [Groves et al. 2000](#ref-Groves2000)). A
survey that feels relevant, that comes from an organisation perceived as
credible and responsive, and where participation feels like it will
amount to something, is experienced as less burdensome regardless of its
length.

Critically, perceived burden is shaped largely before a respondent
encounters the survey itself, by prior experience, reputation, and
whether past participation has felt worthwhile ([Bradburn
1978](#ref-Bradburn1978); [Krosnick 1991](#ref-Krosnick1991)). This
means the levers for managing it extend well beyond survey design and
into how we operate as an organisation over time. It is why what we do
with the data matters as much as how we collect it. Failing to act on
responses is not only a missed opportunity, it is an ethical breach of
the implicit agreement that research participation represents. And in
the communities we work with in the grains sector, where networks are
tight and reputations travel, it has consequences that extend beyond a
single survey to shape how future participation, and future researchers,
are received.

For those interested, these principles and the behavioural and
methodological evidence behind them are explored in depth in
[Behaviourally-Informed Survey Design: A Methodological
Guide](https://AAGI-AUS.github.io/AAGISurvey/behaviourally_informed_survey_design/behaviourally_informed_survey_design.qmd).

### Closing the Loop

Because the value of this survey depends as much on what we do with the
data as on how it is collected, it is worth thinking beyond
distribution. How findings are shared with collaborators, and whether
they can see their input reflected in any changes or communications that
follow, directly affects how worthwhile participating feels and by
extension the quality and consistency of responses over time.

This is an evolving process and there is room to develop it
collectively. If you have ideas about how findings might be acted on,
how insights could be communicated back to collaborators in meaningful
ways, or suggestions that could improve the structure, wording, or
workflow of the survey itself, please share them by emailing
<CBADA@curtin.edu.au> with the subject line *AAGI Collaborator Survey*.

### References

Bradburn, Norman. 1978. “Respondent Burden.” *Proceedings of the Survey
Research Methods Section of the American Statistical Association*,
35–40.

Dillman, Don A., Jolene D. Smyth, and Leah Melani Christian. 2014.
*Internet, Phone, Mail, and Mixed-Mode Surveys: The Tailored Design
Method*. 4th ed. John Wiley & Sons.

Ericson, Bonick, A. 2023. “Optimizing Survey Response Rates in Graduate
Medical Education Research Studies.” *Family Medicine* 55 (5): 304–10.
https://doi.org/<https://doi.org/10.22454/FamMed.2023.750371>.

Groves, Robert M., Eleanor Singer, and Amy Corning. 2000.
“Leverage-Saliency Theory of Survey Participation: Description and an
Illustration.” *Public Opinion Quarterly* 64 (3): 299–308.
<https://doi.org/10.1086/317990>.

Krosnick, Jon A. 1991. “Response Strategies for Coping with the
Cognitive Demands of Attitude Measures in Surveys.” *Applied Cognitive
Psychology* 5 (3): 213–36. <https://doi.org/10.1002/acp.2350050305>.

Kunz, Tanja, and Tobias Gummer. 2024. “Effects of Objective and
Perceived Burden on Response Quality in Web Surveys.” *International
Journal of Social Research Methodology* 27 (3): 385–95.
<https://doi.org/10.1080/13645579.2024.2393795>.

Millar, Morgan M., Hilary A. Hewes, Andrea L. Genovesi, et al. 2021.
“The Effect of the Familiarity of a Survey Sender on Response Outcomes
in a Large-Scale Survey of Emergency Medical Services Agencies.”
*Evaluation & the Health Professions* 44 (3): 260–67.
<https://doi.org/10.1177/01632787211030635>.
