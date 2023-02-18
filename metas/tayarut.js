var data = 
{
  "Author": "צוות אב לתחבורה ירושלים",
  "Publisher": "משרד התחבורה",
  "Contact": "משרד התחבורה",
  "Contact Email": "geo@mot.gov.il",
  "Author Email": "JonathanB@jtmt.gov.il",
  "Title": "סקר הרגלי נסיעה של תיירים 2016-2017",
  "Description": ["סקר הרגלי נסיעה של תיירי חו\"ל נערך בין השנים 2017-2016 ביוזמת צוות תכנית אב לתחבורה בשיתוף עם פרופ' נועם שובל מהמחלקה לגאוגרפיה באוניברסיטה העברית בירושלים.", "הסקר התבצע באמצעות תשאולים פרונטליים שביצעו הסוקרים עם התיירים המגיעים לארץ ובאמצעות מדידת תנועות התיירים בעזרת מכשירי מדידה מתקדמים מבוססי GPS .", "הנתונים שנאספו סונכרנו בזמן אמת למערכת ייעודית שנבנתה עבור סקרי הרגלי נסיעה.", "בנוסף, הסוקרים ביצעו בקרה בזמן אמת על הנתונים, הם פענחו וסיווגו את מיקומי התיירים ואת תנועותיהם במהלך שהותם בארץ וקיימו קשר ישיר עמם ועם מדריכי הקבוצות המאורגנות כדי לאמת את המידע ולהשלים נתונים חסרים."],
  "Created": "22/03/2018",
  "Version": "1",
  "Last updated": "22/03/2018",
  "Temporal coverage": "דצמבר 2015 - נובמבר 2017",
  "Dataset file": "tourism20162017.zip",
  "Metadata creator": "עדו קליין",
  "Metadata creation date": "03/01/2023",
  "Comments": ["הסקר התבצע באופן רציף לאורך כשנתיים, בין החודשים דצמבר 2015 – נובמבר 2017.", "אוכלוסיית הסקר היא תיירים המגיעים מחו\"ל לבקר בישראל בחלוקה לשלושה סוגים:", "תיירים יחידים: תיירים המגיעים לישראל ומתכננים את שהותם בה באופן עצמאי", "קבוצות מאורגנות: תיירים אשר מטיילים בישראל במסגרת קבוצה ומסלול שנקבעו מראש ומלווים במדריך ונהג אוטובוס לאורך כל שהותם בארץ", "קבוצות שיוט: קבוצות המגיעות לארץ דרך הים במסגרת קרוז מאירופה. האוניות של קבוצות השיט עוגנות בנמל אשדוד או נמל חיפה, ומשם התיירים יוצאים למסלול טיול בארץ בהתאם לבחירתם מראש מתוך מספר מסלולים מוצעים.", "מבנה הקבצים הוא כזה שעבור כל סוג, יש טבלת פרטים כלליים וטבלת פעילויות שבוצעו", "פרטים נוספים לגבי אוכולוסית הדגימה והמתודולוגיה מצורפים במסמך המסכם של הסקר", "על מנת לא לחשוף מיקומים מדויקים של תיירים, הקובץ עבר אגרגציה לאיזורי תנועה (על ידי עדו קליין) של שכבת אזורי התנועה של המודל הארצי (2636 אזורי תנועה). מכיוון שלא כל הנקודות הצטלבו עם השכבה, התבצע הטיפול הבא:", "נקודות שנמצאו בסיור בתוך מדינות שכנות (מצרים, ירדן) - צורפו לאזורי תנועה ייעודיים שצורפו לשכבת 2636 (שכעת נקראת שכבת 2640 לאור הוספת 3 אזורי תנועה בירדן ואחד במצרים).", "נקודות שנמצאו בים או סמוך מאוד לגבולות המדינה, שלא בתוך אזור התנועה - שוייכו לאזור התנועה הקרוב ביותר."],
  "Related documents": "דוח סקר הרגלי נסיעה של תיירים מחול.pdf",
  "Keywords": "הרגלי נסיעה, תיירות",
  "Frequency of update": "חד פעמי",
  "Spatial coverage": "ארצי",
  "Language": "אנגלית",
  "Files list": ["individual tourist details.csv", "individual tourist activities.csv", "organized groups details.csv", "organized groups activities.csv", "sail details.csv", "sail activities.csv", "taz_arzi_2640.shp"],
  "Files": [
    {
      "File name": "individual tourist details.csv",
      "File format": "csv",
      "File description": "טבלת פרטים על תיירים יחידים",
      "File fields": [
        {
          "name": "Record_Id",
          "type": "numeric",
          "description": "מזהה יחודי של התייר"
        },
        {
          "name": "organized_group",
          "type": "character",
          "description": "קבוצה מאורגנת",
          "vals": {
            "No": null,
            "Yes": null
          }
        },
        {
          "name": "main_purpose",
          "type": "character",
          "description": "מטרה עיקרית של הביקור",
          "vals": {
            "Business": null,
            "Family and Friends": null,
            "Religion": null,
            "Touring": null
          }
        },
        {
          "name": "secondary_purpose",
          "type": "character",
          "description": "מטרה משנית של הביקור",
          "vals": {
            "Business": null,
            "Family and Friends": null,
            "None": null,
            "Other": null,
            "Religion": null,
            "Touring": null
          }
        },
        {
          "name": "visit_purpose_category",
          "type": "character",
          "description": "סוג מטרת הביקור",
          "vals": {
            "business": null,
            "family & friends": null,
            "traveling and religion": null
          }
        },
        {
          "name": "religious_affiliation",
          "type": "character",
          "description": "שיוך דתי",
          "vals": {
            "Christian": null,
            "Jewish - Other": null,
            "No affiliation": null,
            "Jewish - Religious": null,
            "Other": null,
            "Muslim": null
          }
        },
        {
          "name": "religious_affiliation_category",
          "type": "character",
          "description": "סוג שיוך דתי",
          "vals": {
            "Christian": null,
            "Jewish": null,
            "No affiliation + Other": null,
            "Jewish - Religious": null
          }
        },
        {
          "name": "how_many_nights",
          "type": "numeric",
          "description": "כמות לילות",
          "comment": "לרוב זהה למשתנה number of nights, בפועל יש מספר חריגות מצומצם"
        },
        {
          "name": "length_of_visit",
          "type": "character",
          "description": "אורך הביקור",
          "comment": "ביקור קצר הוא עד 4 ימים, ארוך למעלה מכך",
          "vals": {
            "short visit": null,
            "long visit": null
          }
        },
        {
          "name": "number_of_nights",
          "type": "numeric",
          "description": "כמות לילות",
          "comment": "לרוב זהה למשתנה how many nights, בפועל יש מספר חריגות מצומצם"
        },
        {
          "name": "number_of_days",
          "type": "numeric",
          "description": "כמות ימים"
        },
        {
          "name": "first_visit",
          "type": "character",
          "description": "ביקור ראשון",
          "vals": {
            "No": null,
            "Yes": null
          }
        },
        {
          "name": "Country_of_Residence",
          "type": "character",
          "description": "מדינת מגורים"
        },
        {
          "name": "country_of_origin_category",
          "type": "character",
          "description": "יבשת מגורים",
          "vals": {
            "Europe": null,
            "North America": null,
            "East Asia": null,
            "South and Central America": null,
            "Oceania": null,
            "Africa": null,
            "Other": null
          }
        },
        {
          "name": "Israeli_passport",
          "type": "character",
          "description": "קיום דרכון ישראלי",
          "vals": {
            "No": null,
            "Yes": null,
            "-1": "לא ידוע"
          }
        },
        {
          "name": "How_many_people_in_group",
          "type": "numeric",
          "description": "כמות אנשים בקבוצה"
        },
        {
          "name": "Number_of_people_in_group_category",
          "type": "character",
          "description": "כמות האנשים בקבוצה - תיאור מילולי",
          "vals": {
            "unaccompanied": null,
            "couple": null,
            "above 3": null
          }
        },
        {
          "name": "Number_of_women",
          "type": "numeric",
          "description": "כמות נשים בקבוצה"
        },
        {
          "name": "Number_of_men",
          "type": "numeric",
          "description": "כמות גברים בקבוצה"
        },
        {
          "name": "Summer_Winter",
          "type": "character",
          "description": "עונת הגעה - חורף/קיץ",
          "vals": {
            "Winter": null,
            "Summer": null
          }
        },
        {
          "name": "arrivel_season",
          "type": "character",
          "description": "עונת הגעה",
          "vals": {
            "Christmas": null,
            "Winter": null,
            "Spring": null,
            "Summer": null,
            "Tishrei Holidays": null
          }
        },
        {
          "name": "arrivel_month",
          "type": "numeric",
          "description": "חודש הגעה"
        },
        {
          "name": "Year",
          "type": "numeric",
          "description": "שנה"
        },
        {
          "name": "Tourist_in_organized_group",
          "type": "numeric",
          "description": "תייר בקבוצה מאורגנת",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "number_of_activity_days",
          "type": "numeric",
          "description": "מספר ימי פעילות"
        },
        {
          "name": "total_number_of_activities",
          "type": "numeric",
          "description": "כמות פעילויות"
        },
        {
          "name": "average_number_of_activities_per_day",
          "type": "numeric",
          "description": "ממוצע של פעילויות ליום"
        },
        {
          "name": "average_number_of_activities_per_total_days",
          "type": "numeric",
          "description": "ממוצע של פעילויות לכלל ימי השהייה"
        },
        {
          "name": "average_number_of_activities_per_activity_day",
          "type": "numeric",
          "description": "ממוצע של פעילויות ליום פעילות"
        },
        {
          "name": "First_day_of_activity",
          "type": "date",
          "description": "תאריך היום הראשון לפעילות"
        },
        {
          "name": "Last_day_of_activity",
          "type": "date",
          "description": "תאריך היום האחרון לפעילות"
        },
        {
          "name": "Arrival_date",
          "type": "date",
          "description": "תאריך הגעה"
        },
        {
          "name": "Departure_date",
          "type": "date",
          "description": "תאריך עזיבה"
        },
        {
          "name": "Arrival_weekday",
          "type": "character",
          "description": "יום הגעה בשבוע",
          "vals": {
            "Sunday": null,
            "Monday": null,
            "Tuesday": null,
            "Wednesday": null,
            "Thursday": null,
            "Friday": null,
            "Saturday": null
          }
        },
        {
          "name": "Departure_weekday",
          "type": "character",
          "description": "יום עזיבה בשבוע",
          "vals": {
            "Sunday": null,
            "Monday": null,
            "Tuesday": null,
            "Wednesday": null,
            "Thursday": null,
            "Friday": null,
            "Saturday": null
          }
        },
        {
          "name": "arrival_hour",
          "type": "numeric",
          "description": "שעת הגעה"
        },
        {
          "name": "departure_hour",
          "type": "numeric",
          "description": "שעת עזיבה"
        },
        {
          "name": "arrival_day_period_category",
          "type": "character",
          "description": "תקופת יום הגעה",
          "vals": {
            "Early morning": null,
            "Late morning": null,
            "Early noon": null,
            "Late noon": null,
            "Early evening": null,
            "Late evening": null,
            "night": null,
            "After midnight": null
          }
        },
        {
          "name": "departure_day_period_category",
          "type": "character",
          "description": "תקופת יום עזיבה",
          "vals": {
            "Early morning": null,
            "Late morning": null,
            "Early noon": null,
            "Late noon": null,
            "Early evening": null,
            "Late evening": null,
            "night": null,
            "After midnight": null
          }
        },
        {
          "name": "activities_on_first_day",
          "type": "numeric",
          "description": "קיום פעילות ביום הראשון לביקור",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "activities_on_last_day",
          "type": "numeric",
          "description": "קיום פעילות ביום האחרון לביקור",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "Number_of_accommodations",
          "type": "numeric",
          "description": "מספר מקומות מגורים"
        },
        {
          "name": "other_transportation_use",
          "type": "numeric",
          "description": "שימוש בתחבורה מסוג אחר",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "organized_transportation_use",
          "type": "numeric",
          "description": "שימוש בהסעה מאורגנת",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "taxi_use",
          "type": "numeric",
          "description": "שימוש במונית",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "private_vehicle_use",
          "type": "numeric",
          "description": "שימוש ברכב פרטי",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "public_transportation_use",
          "type": "numeric",
          "description": "שימוש בתחבורה ציבורית",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "precentage_of_other_transportation_use",
          "type": "character",
          "description": "אחוז השימוש בתחבורה מסוג אחר"
        },
        {
          "name": "Percentage_of_organized_transportation_use",
          "type": "character",
          "description": "אחוז השימוש בהסעה מאורגנת"
        },
        {
          "name": "Percentage_of_taxi_use",
          "type": "character",
          "description": "אחוז השימוש במונית"
        },
        {
          "name": "Percentage_of_private_care_use",
          "type": "character",
          "description": "אחוז השימוש ברכב פרטי"
        },
        {
          "name": "Percentage_of_public_transportation_use",
          "type": "character",
          "description": "אחוז השימוש בתחבורה ציבורית"
        },
        {
          "name": "number_of_days_in_Jerusalem",
          "type": "numeric",
          "description": "כמות ימים בהם התייר הייתה בירושלים"
        },
        {
          "name": "Percentage_of_days_in_Jerusalem",
          "type": "character",
          "description": "אחוז הימים בהם התיירהייתה בירושלים"
        },
        {
          "name": "Visit_to_Jordan",
          "type": "numeric",
          "description": "האם התקיימה גיחה לירדן",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "number_of_Days_in_Jordan",
          "type": "numeric",
          "description": "כמות הימים בירדן"
        },
        {
          "name": "Days_without_activities",
          "type": "numeric",
          "description": "קיום ימים ללא פעילות",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "number_of_days_without_activities",
          "type": "numeric",
          "description": "כמות הימים ללא פעילות"
        },
        {
          "name": "percentage_of_days_without_activities",
          "type": "character",
          "description": "אחוז הימים ללא פעילות מכלל הימים"
        },
        {
          "name": "Returned_to_accommodation_during_the_day",
          "type": "numeric",
          "description": "חזרה למגורים תוך כדי היום",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "Total_retunings_during_the_stay",
          "type": "numeric",
          "description": "כמות כוללת של חזרות למגורים במהלך הביקור"
        },
        {
          "name": "average_amount_of_returnings",
          "type": "numeric",
          "description": "זמן שהייה ממוצע בחזרה למגורים"
        },
        {
          "name": "number_of_days_using_organized_transportation",
          "type": "numeric",
          "description": "כמות ימים בהם נעשה שימוש בתחבורה מאורגנת"
        },
        {
          "name": "percentage_usage_of_organized_transportation",
          "type": "character",
          "description": "אחוז השימוש בתחבורה מאורגנת"
        },
        {
          "name": "Weighting_factor",
          "type": "numeric",
          "description": "מקדם ניפוח"
        },
        {
          "name": "Weighting_factor_excluding_accompanied",
          "type": "numeric",
          "description": "מקדם ניפוח בהורדת מלווים"
        },
        {
          "name": "female_Weighting_factor",
          "type": "numeric",
          "description": "מקדם ניפוח לנשים"
        },
        {
          "name": "male_Weighting_factor",
          "type": "numeric",
          "description": "מקדם ניפוח לגברים"
        },
        {
          "name": "male_age_0_12",
          "type": "numeric",
          "description": "כמות גברים בגילאים 0-12"
        },
        {
          "name": "male_age_13_18",
          "type": "numeric",
          "description": "כמות גברים בגילאים 13-18"
        },
        {
          "name": "male_age_19_24",
          "type": "numeric",
          "description": "כמות גברים בגילאים 19-24"
        },
        {
          "name": "male_age_25_39",
          "type": "numeric",
          "description": "כמות גברים בגילאים 25-39"
        },
        {
          "name": "male_age_40_59",
          "type": "numeric",
          "description": "כמות גברים בגילאים 40-59"
        },
        {
          "name": "male_age_60_74",
          "type": "numeric",
          "description": "כמות גברים בגילאים 60-74"
        },
        {
          "name": "male_age_75plus",
          "type": "numeric",
          "description": "כמות גברים בגילאים 75+"
        },
        {
          "name": "female_age_0_12",
          "type": "numeric",
          "description": "כמות נשים בגילאים 0-12"
        },
        {
          "name": "female_age_13_18",
          "type": "numeric",
          "description": "כמות נשים בגילאים 13-18"
        },
        {
          "name": "female_age_19_24",
          "type": "numeric",
          "description": "כמות נשים בגילאים 19-24"
        },
        {
          "name": "female_age_25_39",
          "type": "numeric",
          "description": "כמות נשים בגילאים 25-39"
        },
        {
          "name": "female_age_40_59",
          "type": "numeric",
          "description": "כמות נשים בגילאים 40-59"
        },
        {
          "name": "female_age_60_74",
          "type": "numeric",
          "description": "כמות נשים בגילאים 60-74"
        },
        {
          "name": "female_age_75plus",
          "type": "numeric",
          "description": "כמות נשים בגילאים 75+"
        },
        {
          "name": "male_weighiting_factor_0_12",
          "type": "numeric",
          "description": "מקדם ניפוח לגברים בגילאים 0-12"
        },
        {
          "name": "male_weighiting_factor_13_18",
          "type": "numeric",
          "description": "מקדם ניפוח לגברים בגילאים 13-18"
        },
        {
          "name": "male_weighiting_factor_19_24",
          "type": "numeric",
          "description": "מקדם ניפוח לגברים בגילאים 19-24"
        },
        {
          "name": "male_weighiting_factor_25_39",
          "type": "numeric",
          "description": "מקדם ניפוח לגברים בגילאים 25-39"
        },
        {
          "name": "male_weighiting_factor_40_59",
          "type": "numeric",
          "description": "מקדם ניפוח לגברים בגילאים 40-59"
        },
        {
          "name": "male_weighiting_factor_60_74",
          "type": "numeric",
          "description": "מקדם ניפוח לגברים בגילאים 60-74"
        },
        {
          "name": "male_weighiting_factor_75plus",
          "type": "numeric",
          "description": "מקדם ניפוח לגברים בגילאים 75+"
        },
        {
          "name": "female_weighiting_factor_0_12",
          "type": "numeric",
          "description": "מקדם ניפוח לנשים בגילאים 0-12"
        },
        {
          "name": "female_weighiting_factor_13_18",
          "type": "numeric",
          "description": "מקדם ניפוח לנשים בגילאים 13-18"
        },
        {
          "name": "female_weighiting_factor_19_24",
          "type": "numeric",
          "description": "מקדם ניפוח לנשים בגילאים 19-24"
        },
        {
          "name": "female_weighiting_factor_25_39",
          "type": "numeric",
          "description": "מקדם ניפוח לנשים בגילאים 25-39"
        },
        {
          "name": "female_weighiting_factor_40_59",
          "type": "numeric",
          "description": "מקדם ניפוח לנשים בגילאים 40-59"
        },
        {
          "name": "female_weighiting_factor_60_74",
          "type": "numeric",
          "description": "מקדם ניפוח לנשים בגילאים 60-74"
        },
        {
          "name": "female_weighiting_factor_75plus",
          "type": "numeric",
          "description": "מקדם ניפוח לנשים בגילאים 75+"
        }
      ]
    },
    {
      "File name": "individual tourist activities.csv",
      "File format": "csv",
      "File description": "טבלת פעילויות של תיירים יחידים",
      "File fields": [
        {
          "name": "Record_ID",
          "type": "numeric",
          "description": "מזהה רשומת תייר"
        },
        {
          "name": "Activity_ID",
          "type": "numeric",
          "description": "מזהה רשומת פעילות"
        },
        {
          "name": "Travel_Date",
          "type": "date",
          "description": "תאריך ביצוע הפעילות"
        },
        {
          "name": "Place_Number",
          "type": "numeric",
          "description": "מיקום הפעילות בסדר היומי"
        },
        {
          "name": "Activity_Name",
          "type": "character",
          "description": "שם מיקום הפעילות"
        },
        {
          "name": "Arriving_Time",
          "type": "character",
          "description": "שעת הגעה"
        },
        {
          "name": "Departure_Time",
          "type": "character",
          "description": "שעת עזיבה"
        },
        {
          "name": "TAZ_2640",
          "type": "numeric",
          "description": "מספר אזור התנועה בשכבת 2640 אזורי תנועה בו בוצעה הפעילות"
        },
        {
          "name": "activity_purpose",
          "type": "character",
          "description": "מטרת הפעילות",
          "vals": {
            "Arrival /Departure Via Ben Gurion Airport": null,
            "Shopping / Arrangements / Food": null,
            "Changing means of transportation": null,
            "Hotel / Holiday house / Residence": null,
            "Work / Work related / Study / Volunteering": null,
            "Tourism / leisure / religious services / sports activities": null,
            "Family visit / friends": null,
            "pick up/let off passenger": null,
            "Health Services": null,
            "Located in Jordan / Egypt": null
          }
        },
        {
          "name": "means_of_transportation",
          "type": "character",
          "description": "אמצעי הגעה לפעילות",
          "vals": {
            "NA": null,
            "walk": null,
            "Private car": null,
            "Public Transport": null,
            "Taxi": null,
            "Organized transportation": null,
            "Other": null,
            "-1": null
          }
        },
        {
          "name": "activity_order",
          "type": "numeric",
          "description": "סדר הפעילות בתוך כלל פעילויות התיירים"
        },
        {
          "name": "visit_purpose_category",
          "type": "character",
          "description": "קטגורית מטרת הפעילות",
          "vals": {
            "business": null,
            "family & friends": null,
            "traveling and religion": null
          }
        },
        {
          "name": "religious_affiliation_category",
          "type": "character",
          "description": "קטגורית שיוך דתי של התייר",
          "vals": {
            "Christian": null,
            "Jewish": null,
            "No affiliation + Other": null,
            "Jewish - Religious": null
          }
        },
        {
          "name": "first_visit",
          "type": "character",
          "description": "האם ביקור ראשון של התייר בישראל",
          "vals": {
            "no": null,
            "yes": null
          }
        },
        {
          "name": "country_of_origin",
          "type": "character",
          "description": "יבשת מוצא",
          "vals": {
            "Europe": null,
            "North America": null,
            "East Asia": null,
            "South and Central America": null,
            "Oceania": null,
            "Africa": null,
            "Other": null
          }
        },
        {
          "name": "Number_of_people_in_group_category",
          "type": "character",
          "description": "כמות האנשים בקבוצה - תיאור מילולי",
          "vals": {
            "unaccompanied": null,
            "couple": null,
            "above 3": null
          }
        },
        {
          "name": "length_of_visit",
          "type": "character",
          "description": "אורך הביקור - תיאור מילולי",
          "vals": {
            "short visit": null,
            "long visit": null
          }
        },
        {
          "name": "Summer_Winter",
          "type": "character",
          "description": "חורף / קיץ",
          "vals": {
            "Winter": null,
            "Summer": null
          }
        },
        {
          "name": "arrival_season",
          "type": "character",
          "description": "עונת הגעה",
          "vals": {
            "Christmas": null,
            "Winter": null,
            "Spring": null,
            "Summer": null,
            "Tishrei 'Holidays": null
          }
        },
        {
          "name": "Tourist_in_organized_group",
          "type": "numeric",
          "description": "האם התייר בקבוצה מאורגנת",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "Record_Id_plus_travel_day",
          "type": "numeric",
          "description": "מזהה רשומת תייר ויום",
          "comment": "מספר המורכב ממזהה רשומת התייר שלאחריו ללא רווח מצויינים כמות הימים מ1.1.1899",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "Activities_for_analysis",
          "type": "numeric",
          "description": "האם פעילות לניתוח",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "First_activity_for_tourist",
          "type": "numeric",
          "description": "האם פעילות ראשונה של התייר",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "Last_activity_for_tourist",
          "type": "numeric",
          "description": "האם פעולות אחרונה של התייר",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "first_activity_of_the_day",
          "type": "numeric",
          "description": "האם פעילות ראשונה ביום",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "last_activity_of_the_day",
          "type": "numeric",
          "description": "האם פעילות אחרונה ביום",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "activity_during_the_day",
          "type": "numeric",
          "description": "האם פעילות אמצעית ביום",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "first_activity_for_analysis",
          "type": "numeric",
          "description": "האם פעילות ראשונה לניתוח",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "last_activity_for_analysis",
          "type": "numeric",
          "description": "האם פעילות אחרונה לניתוח",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "first_accommodation_activity",
          "type": "numeric",
          "description": "האם פעילות מגורים ראשונה",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "last_accommodation_activity",
          "type": "numeric",
          "description": "האם פעילות מגורים אחרונה",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "mid_day_accommodation_activity",
          "type": "numeric",
          "description": "האם פעילות מגורים של אמצע היום",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "arrival_date",
          "type": "character",
          "description": "תאריך הגעה"
        },
        {
          "name": "departure_date",
          "type": "character",
          "description": "תאריך עזיבה"
        },
        {
          "name": "number_of_nights",
          "type": "numeric",
          "description": "כמות לילות"
        },
        {
          "name": "total_number_of_days",
          "type": "numeric",
          "description": "כמות ימים כוללת"
        },
        {
          "name": "number_of_activity_days",
          "type": "numeric",
          "description": "מספר ימי פעילות"
        },
        {
          "name": "day_order",
          "type": "numeric",
          "description": "סדר יום הפעילות"
        },
        {
          "name": "day_position_from_total_days",
          "type": "numeric",
          "description": "אחוז יום הפעילות"
        },
        {
          "name": "rounded_day_order",
          "type": "numeric",
          "description": "יום פעילות מעודל"
        },
        {
          "name": "day_order_all_days_category",
          "type": "numeric",
          "description": "קטגורית סדר יום פעילות"
        },
        {
          "name": "first_day_in_the_country",
          "type": "numeric",
          "description": "האם יום ראשון במדינה",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "last_day_in_the_country",
          "type": "numeric",
          "description": "האם יום אחרון במדינה",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "one_day_in_accommodation_only",
          "type": "numeric",
          "description": "האם יום אחד בלבד במגורים",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "number_of_activities_throughout_the_trip",
          "type": "numeric",
          "description": "כמות פעילויות במהלך הביקור"
        },
        {
          "name": "number_of_activities_per_day_for_analysis",
          "type": "numeric",
          "description": "כמות פעילות לניתוח במהלך היום"
        },
        {
          "name": "activity_weekday",
          "type": "character",
          "description": "יום הפעילות בשבוע",
          "vals": {
            "Sunday": null,
            "Monday": null,
            "Tuesday": null,
            "Wednesday": null,
            "Thursday": null,
            "Friday": null,
            "Saturday": null
          }
        },
        {
          "name": "activity_month",
          "type": "numeric",
          "description": "חודש הפעילות"
        },
        {
          "name": "year",
          "type": "numeric",
          "description": "שנת הפעילות"
        },
        {
          "name": "returned_to_accommodation_during_the_day",
          "type": "numeric",
          "description": "האם פעילות חזרה למגורים",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "returns_per_day",
          "type": "numeric",
          "description": "כמות חזרות ביום"
        },
        {
          "name": "Total_activity_duration",
          "type": "character",
          "description": "אורך הפעילות בפורמט hh:mm"
        },
        {
          "name": "time_between_activities",
          "type": "character",
          "description": "מרווח הזמן עד הפעילות הבאה ביום בפורמט hh:mm"
        },
        {
          "name": "total_minutes_of_activity",
          "type": "numeric",
          "description": "כמות דקות כוללת בפעילות"
        },
        {
          "name": "Arriving_Time_minutes",
          "type": "numeric",
          "description": "דקת הגעה"
        },
        {
          "name": "Arriving_Time_hour",
          "type": "numeric",
          "description": "שעת הגעה"
        },
        {
          "name": "Departure_Time_minutes",
          "type": "numeric",
          "description": "דקת יציאה"
        },
        {
          "name": "Departure_Time_hour",
          "type": "numeric",
          "description": "שעת יציאה"
        },
        {
          "name": "arrival_hour",
          "type": "numeric",
          "description": "שעת הגעה"
        },
        {
          "name": "departure_hour",
          "type": "numeric",
          "description": "שעת יציאה"
        },
        {
          "name": "arrival_day_period_category",
          "type": "character",
          "description": "תקופת הגעה - תאור מילולי",
          "vals": {
            "Late noon": null,
            "Late evening": null,
            "Early noon": null,
            "Early evening": null,
            "night": null,
            "Early morning": null,
            "Late morning": null,
            "After midnight": null
          }
        },
        {
          "name": "departure_day_period_category",
          "type": "character",
          "description": "תקופת יציאה - תאור מילולי",
          "vals": {
            "Late noon": null,
            "Late evening": null,
            "Early noon": null,
            "Early evening": null,
            "night": null,
            "Early morning": null,
            "Late morning": null,
            "After midnight": null
          }
        },
        {
          "name": "Municipal_Area_layer_Source",
          "type": "character",
          "description": "שם מיקום הפעילות ברמת יישוב"
        },
        {
          "name": "Tourist_Area_layer_Source",
          "type": "character",
          "description": "שם מיקום הפעילות בשכבת התיירים"
        },
        {
          "name": "tourist_final_aria",
          "type": "character",
          "description": "שם מיקום הפעילות ברמת אזור סופי"
        },
        {
          "name": "Tourist_sites_excluding_city",
          "type": "character",
          "description": "שם תיירותי של מקום הפעילות"
        },
        {
          "name": "country_area",
          "type": "character",
          "description": "איזור בישראל",
          "vals": {
            "Central Israel": null,
            "Tel Aviv": null,
            "North": null,
            "The Dead Sea": null,
            "Jerusalem and around": null,
            "Judaea and Samaria": null,
            "South": null,
            "Not in Israel": null
          }
        },
        {
          "name": "Jerusalem_traffic_area_number",
          "type": "numeric",
          "description": "מספר אזור תנועה בירושלים"
        },
        {
          "name": "Multi_zone_traffic_area_name_Jerusalem",
          "type": "character",
          "description": "שם אזור על תנועה בירושלים"
        },
        {
          "name": "Place_of_accomadation_in_Jerusalem",
          "type": "character",
          "description": "מקום מגורים בירושלים",
          "vals": {
            "NA": null,
            "other": null,
            "city center": null,
            "Emek Refaim": null,
            "Nahlaot": null,
            "Ramat Eshkol": null,
            "City entrance": null,
            "the old City": null,
            "The American Colony": null
          }
        },
        {
          "name": "Morning_accommodation_location",
          "type": "character",
          "description": "שם מקום מגורים בבוקר"
        },
        {
          "name": "Evening_accommodation_location",
          "type": "character",
          "description": "שם מקום מגורים בערב"
        },
        {
          "name": "same_accommodation",
          "type": "numeric",
          "description": "האם מגורים באותו המקום בבוקר ובערב",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "Municipal_Morning_accommodation_location",
          "type": "character",
          "description": "רשות מקומית בה התבצעו המגורים בבוקר"
        },
        {
          "name": "Jerusalem_day_trip",
          "type": "numeric",
          "description": "האם יום ביקור בירושלים",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "Sea_of_Galilee_day_trip",
          "type": "numeric",
          "description": "האם יום ביקור בכנרץ",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "Tel_Aviv_day_trip",
          "type": "numeric",
          "description": "האם יום ביקור בתל אביב",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "Nazareth_day_trip",
          "type": "numeric",
          "description": "האם יום ביקור בנצרת",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "Dead_sea_day_trip",
          "type": "numeric",
          "description": "האם יום ביקור בים המלח",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "Haifa_day_trip",
          "type": "numeric",
          "description": "האם יום ביקור בחיפה",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "Bethlehem_day_trip",
          "type": "numeric",
          "description": "האם יום ביקור בבית לחם",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "Caesarea_day_trip",
          "type": "numeric",
          "description": "האם יום ביקור בקיסריה",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "Acco_day_trip",
          "type": "numeric",
          "description": "האם יום ביקור בעכו",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "organized_day_trip",
          "type": "character",
          "description": "האם יום עם טיול מאורגן",
          "vals": {
            "No": null,
            "-1": null,
            "Yes": null
          }
        },
        {
          "name": "number_of_days_using_organized_transportation",
          "type": "numeric",
          "description": "כמות הימים בהם נעשה שימוש בהסעה מאורגנת"
        },
        {
          "name": "percentage_usage_of_organized_transportation",
          "type": "character",
          "description": "אחוז השימוש בהסעה מאורגנת"
        },
        {
          "name": "Jordan_full_day",
          "type": "numeric",
          "description": "האם הפעילות היא ביום שלם בירדן",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "Jordan_partial_day",
          "type": "numeric",
          "description": "האם הפעילות היא ביום חלקי בירדן",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "visit_to_Jordan",
          "type": "numeric",
          "description": "האם היה ביקור בירדן",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "Weighting_factor",
          "type": "numeric",
          "description": "מקדם ניפוח"
        },
        {
          "name": "sum_of_weighted_activities",
          "type": "numeric",
          "description": "סכום הפעילויות המנופחות"
        },
        {
          "name": "private_vehicle_use",
          "type": "numeric",
          "description": "האם התייר עשה שימוש ברכב פרטי",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "organized_transportation_ues",
          "type": "numeric",
          "description": "האם התייר עשה שימוש בהסעה מאורגנת",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "public_transportation_use",
          "type": "numeric",
          "description": "האם התייר עשה שימוש בתחבורה ציבורית",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "precentage_other",
          "type": "character",
          "description": "שיעור שימוש בתחבורה מסוג אחר"
        },
        {
          "name": "precentage_walking",
          "type": "character",
          "description": "שיעור הליכה"
        },
        {
          "name": "precentage_organized_transportation",
          "type": "character",
          "description": "שיעור שימוש בהסעה מאורגנת"
        },
        {
          "name": "precentage_taxi",
          "type": "character",
          "description": "שיעור שימוש במונית"
        },
        {
          "name": "precentage_private_vehicle",
          "type": "character",
          "description": "שיעור שימוש ברכב פרטי"
        },
        {
          "name": "precentage_public_transportation",
          "type": "character",
          "description": "שיעור שימוש בתחבורה ציבורית"
        },
        {
          "name": "precentage_other_excluding_walking",
          "type": "character",
          "description": "שיעוש שימוש בתחבורה מסוג אחר שאינה הליכה"
        },
        {
          "name": "precentage_organized_transportation_excluding_walking",
          "type": "character",
          "description": "שיעור שימוש בהסעה מאורגנת ללא הליכה"
        },
        {
          "name": "precentage_taxi_excluding_walking",
          "type": "character",
          "description": "שיעור שימוש במונית ללא הליכה"
        },
        {
          "name": "precentage_private_vehicle_excluding_walking",
          "type": "character",
          "description": "שיעור שימוש ברכב פרטי ללא הליכה"
        },
        {
          "name": "precentage_public_transportation_excluding_walking",
          "type": "character",
          "description": "שיעור שימוש בתחבורה ציבורית ללא הליכה"
        },
        {
          "name": "day_category",
          "type": "character",
          "description": "סוג היום בסדר הימים",
          "vals": {
            "First day": null,
            "Normal day": null,
            "Last day": null
          }
        },
        {
          "name": "day_order_Jerusalem",
          "type": "numeric",
          "description": "מספר היום בסדר הימים שהתייר בילה בירושלים"
        },
        {
          "name": "days_in_Jerusalem",
          "type": "numeric",
          "description": "כמות ימים בירושלים"
        },
        {
          "name": "Jerusalem_visiting_category",
          "type": "character",
          "description": "כמות ימים בירושלים - תיאור מילולי",
          "vals": {
            "0": null,
            "More than one day": null,
            "One day": null
          }
        },
        {
          "name": "light_rail_use",
          "type": "numeric",
          "description": "האם היה שימוש ברכבת הקלה בירושלים בפעילות",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "Distance_between_accommodation_and_activities_km",
          "type": "numeric",
          "description": "מרחק בין המגורים לפעילות בק\"מ"
        },
        {
          "name": "accommodation_combination",
          "type": "character",
          "description": "שילוב מגורים של התייר"
        }
      ]
    },
    {
      "File name": "organized groups details.csv",
      "File format": "csv",
      "File description": "טבלת פרטים של קבוצות מאורגנות",
      "File fields": [
        {
          "name": "Record_Id",
          "type": "numeric",
          "description": "מזהה רשומה"
        },
        {
          "name": "traveling_company",
          "type": "character",
          "description": "סוכנות נסיעות"
        },
        {
          "name": "Group_name",
          "type": "character",
          "description": "שם הקבוצה"
        },
        {
          "name": "groups_Religious_affiliation",
          "type": "character",
          "description": "שיוך דתי",
          "vals": {
            "mixed group": null,
            "Protestant": null,
            "Jewish": null,
            "Catholic": null,
            "Orthodox Christian": null,
            "NA": null
          }
        },
        {
          "name": "Number_of_days_of_stay",
          "type": "numeric",
          "description": "מספר ימי ביקור"
        },
        {
          "name": "arrival_hour",
          "type": "character",
          "description": "זמן הגעה בפורמט hh:mm:ss"
        },
        {
          "name": "departure_Hour",
          "type": "character",
          "description": "זמן יציאה בפורמט hh:mm:ss"
        },
        {
          "name": "groups_meeting_place",
          "type": "character",
          "description": "מיקום פגישת הקבוצה",
          "vals": {
            "Ben Gurion Airport": null,
            "hotel": null,
            "Border Crossing": null,
            "NA": null
          }
        },
        {
          "name": "groups_departure_place",
          "type": "character",
          "description": "מיקום יציאת הקבוצה",
          "vals": {
            "Ben Gurion Airport": null,
            "hotel": null,
            "Border Crossing": null,
            "NA": null
          }
        },
        {
          "name": "size_of_all_the_tour_groups",
          "type": "numeric",
          "description": "גודל כלל הקבוצות המבקרות יחד עם הקבוצה הנדגמת"
        },
        {
          "name": "Individual_Group_size",
          "type": "numeric",
          "description": "גודל הקבוצה הנדגמת"
        },
        {
          "name": "group_size_category",
          "type": "character",
          "description": "גודל הקבוצה  - בצורה מילולית",
          "vals": {
            "1-10": null,
            "10-20": null,
            "20-30": null,
            "30-40": null,
            "40-50": null,
            "50+": null,
            "NA": null
          }
        },
        {
          "name": "average_group_size_on_bus",
          "type": "numeric",
          "description": "גודל קבוצה ממוצעת על אוטובוס"
        },
        {
          "name": "number_of_Buses",
          "type": "numeric",
          "description": "מספר האוטובוסים בקבוצה"
        },
        {
          "name": "Country_of_Origin",
          "type": "character",
          "description": "ארץ מוצא"
        },
        {
          "name": "Country_of_Origin_Category",
          "type": "character",
          "description": "יבשת מוצא",
          "vals": {
            "North America": null,
            "Europe": null,
            "Oceania": null,
            "East Asia": null,
            "Africa": null,
            "South and Central America": null,
            "NA": null
          }
        },
        {
          "name": "secondary_country_of_origin",
          "type": "character",
          "description": "ארץ מוצא נוספת"
        },
        {
          "name": "arrival_date",
          "type": "character",
          "description": "תאריך הגעה"
        },
        {
          "name": "departure_date",
          "type": "character",
          "description": "תאריך יציאה"
        },
        {
          "name": "number_of_stay_days_by_activities",
          "type": "numeric",
          "description": "מספר ימי פעילות"
        },
        {
          "name": "length_of_visit",
          "type": "character",
          "description": "אורך הביקור - בצורה מילולית",
          "vals": {
            "long visit": null,
            "short visit": null,
            "NA": null
          }
        },
        {
          "name": "month_of_arrival",
          "type": "numeric",
          "description": "חודש הגעה"
        },
        {
          "name": "Summer_Winter",
          "type": "character",
          "description": "חורף/קיץ",
          "vals": {
            "Summer": null,
            "Winter": null,
            "NA": null
          }
        },
        {
          "name": "arrival_season",
          "type": "character",
          "description": "עונת הגעה",
          "vals": {
            "Spring": null,
            "Summer": null,
            "Tishrei 'Holidays": null,
            "Christmas": null,
            "Winter": null,
            "NA": null
          }
        },
        {
          "name": "Arrival_weekday",
          "type": "character",
          "description": "יום בשבוע בו הקבוצה הגיעה",
          "vals": {
            "Sunday": null,
            "Monday": null,
            "Tuesday": null,
            "Wednesday": null,
            "Thursday": null,
            "Friday": null,
            "Saturday": null,
            "NA": null
          }
        },
        {
          "name": "Departure_weekday",
          "type": "character",
          "description": "יום בשבוע בו הקבוצה יצאה",
          "vals": {
            "Sunday": null,
            "Monday": null,
            "Tuesday": null,
            "Wednesday": null,
            "Thursday": null,
            "Friday": null,
            "Saturday": null,
            "NA": null
          }
        },
        {
          "name": "number_of_activities_all_trip",
          "type": "numeric",
          "description": "כמות הפעילות בכל הביקור"
        },
        {
          "name": "average_number_of_activities_per_day",
          "type": "numeric",
          "description": "ממוצע פעילויות ליום"
        },
        {
          "name": "First_day_of_activity",
          "type": "date",
          "description": "תאריך פעילות ראשונה"
        },
        {
          "name": "Last_day_of_activity",
          "type": "date",
          "description": "תאריך פעילות אחרונה"
        },
        {
          "name": "number_of_activity_days",
          "type": "numeric",
          "description": "כמות ימי פעילות"
        },
        {
          "name": "activities_on_first_day",
          "type": "numeric",
          "description": "האם היו פעילויות ביום הראשון",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "activities_on_last_day",
          "type": "numeric",
          "description": "האם היו פעילויות ביום האחרון",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "returend_to_accommodation",
          "type": "numeric",
          "description": "האם שבו למגורים",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "Total_returenings_to_accommodation",
          "type": "numeric",
          "description": "כמת שיבות למגורים"
        },
        {
          "name": "average_returenings_to_accommodation",
          "type": "numeric",
          "description": "ממוצע שיבות למגורים ליום"
        },
        {
          "name": "didnt_sleep_in_Jerusalem",
          "type": "numeric",
          "description": "האם לא לנו בירושלים",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "number_of_accommodations",
          "type": "numeric",
          "description": "כמות מקומות לינה"
        },
        {
          "name": "average_nights_at_accommodation",
          "type": "numeric",
          "description": "כמות לילות ממוצעת ללינה"
        },
        {
          "name": "number_of_free_days",
          "type": "numeric",
          "description": "כמות ימים חופשיים"
        },
        {
          "name": "free_day_location",
          "type": "character",
          "description": "מיקום היום החופשי"
        },
        {
          "name": "group_arrives_independently_on_first_day_of_tour",
          "type": "character",
          "description": "האם הקבוצה הגיעה בצורה עצמאית ביום הראשון",
          "vals": {
            "-1": null,
            "no": null,
            "yes": null,
            "NA": null
          }
        },
        {
          "name": "group_arrives_independently_to_air_port_on_last_day_of_stay",
          "type": "character",
          "description": "האם הקבוצה הגיעה לשדה התעופה בצורה עצמאית ביום האחרון",
          "vals": {
            "-1": null,
            "no": null,
            "yes": null,
            "NA": null
          }
        },
        {
          "name": "number_of_days_in_Jerusalem",
          "type": "numeric",
          "description": "כמות ימים בירושלים"
        },
        {
          "name": "Factor",
          "type": "numeric",
          "description": "מקדם ניפוח"
        }
      ]
    },
    {
      "File name": "organized groups activities.csv",
      "File format": "csv",
      "File description": "טבלת פעילויות של קבוצות מאורגנות",
      "File fields": [
        {
          "name": "Record_ID",
          "type": "numeric",
          "description": "מזהה רשומה"
        },
        {
          "name": "Member_ID",
          "type": "numeric",
          "description": "מזהה חבר קבוצה"
        },
        {
          "name": "Activity_ID",
          "type": "numeric",
          "description": "מזהה פעילות"
        },
        {
          "name": "Travel_Day",
          "type": "date",
          "description": "תאריך הפעילות"
        },
        {
          "name": "Place_Number",
          "type": "numeric",
          "description": "מספר הפעילות"
        },
        {
          "name": "Activity_Name",
          "type": "character",
          "description": "שם מקום הפעילות"
        },
        {
          "name": "Arriving_Time",
          "type": "difftime",
          "description": "שעת הגעה",
          "comment": "מתאפס בשלוש בבוקר - במידה והתחיל יום חדש, תהיה הגעה לפעילות בשלוש בבוקר"
        },
        {
          "name": "Departure_Time",
          "type": "difftime",
          "description": "שעת עזיבה",
          "comment": "מתאפס בשלוש בבוקר - במידה והתחיל יום חדש, תהיה עזיבת פעילות בשלוש בבוקר"
        },
        {
          "name": "Original_Arriving_Time",
          "type": "difftime",
          "description": "שעת הגעה לא ערוכה"
        },
        {
          "name": "Original_Departure_Time",
          "type": "difftime",
          "description": "שעת עזיבה לא ערוכה"
        },
        {
          "name": "TAZ_2640",
          "type": "numeric",
          "description": "מספר אזור התנועה בשכבת 2640 אזורי תנועה בו בוצעה הפעילות"
        },
        {
          "name": "Purpose_of_activity",
          "type": "character",
          "description": "מטרת הפעילות",
          "vals": {
            "Group arrival / departure": null,
            "Hotel / Holiday house / Residence": null,
            "Tourism / leisure / religious services / sports activities": null,
            "Changing means of transportation": null,
            "Shopping / Arrangements / Food": null,
            "Free activity of the group": null,
            "pick up/let off passenger": null,
            "Parking in the bus parking lot": null,
            "parking": null,
            "DriverPersonal Activity": null,
            "Gas station / maintenance of the bus": null,
            "Health services": null,
            "Work / Volunteer": null,
            "Located in Jordan / Egypt": null,
            "-1": null
          }
        },
        {
          "name": "means_of_transport",
          "type": "character",
          "description": "אמצעי הגעה לפעילות",
          "vals": {
            "NA": null,
            "groups bus": null,
            "walk": null,
            "Public Transport": null,
            "other": null,
            "Organized transportation": null,
            "Private car": null,
            "-1": null
          }
        },
        {
          "name": "comment",
          "type": "character",
          "description": "הערות לגבי קליטת הפעילות"
        },
        {
          "name": "order_of_activities",
          "type": "numeric",
          "description": "פעילות לפי סדר כרונולוגי בתוך כלל הפעילויות",
          "comment": "האינדקס מתווסף לפי תאריך הגעת הקבוצה, יתכנו קפיצות אחורה בזמן"
        },
        {
          "name": "record_ID_plus_Member_Id",
          "type": "numeric",
          "description": "שילוב של מזהה הקבוצה ומזהה חבר הקבוצה"
        },
        {
          "name": "Record_Id_plus_travel_day",
          "type": "numeric",
          "description": "שילוב של מזהה הקבוצה ומזהה כמות ימים מהתאריך הראשון באקסל",
          "comment": "התאריך הראשון הוא מתישהו ב1899"
        },
        {
          "name": "ID_plus_MEMBER_plus_DAY",
          "type": "numeric",
          "description": "שילוב של מזהה הקבוה, חבר הקבוצה ומזהה כמות ימים"
        },
        {
          "name": "groups_Religious_affiliation",
          "type": "character",
          "description": "שיוך דתי של הקבוצה",
          "vals": {
            "mixed group": null,
            "Protestant": null,
            "Jewish": null,
            "Catholic": null,
            "Orthodox Christian": null
          }
        },
        {
          "name": "Summer_Winter",
          "type": "character",
          "description": "חורף / קיץ",
          "vals": {
            "Summer": null,
            "Winter": null
          }
        },
        {
          "name": "arrival_season",
          "type": "character",
          "description": "עונת הגעה",
          "vals": {
            "Spring": null,
            "Summer": null,
            "Tishrei 'Holidays": null,
            "Christmas": null,
            "Winter": null
          }
        },
        {
          "name": "group_size_category",
          "type": "character",
          "description": "קטגורית גודל קבוצה",
          "comment": "יש בעייה עם חלק מהערכים המזוהים בטעות כתאריך בגלל המרה שגויה של אקסל",
          "vals": {
            "1-10": null,
            "10-20": null,
            "20-30": null,
            "30-40": null,
            "40-50": null,
            "50+": null
          }
        },
        {
          "name": "number_days_of_stay",
          "type": "numeric",
          "description": "כמות ימים של שהייה בארץ"
        },
        {
          "name": "activities_for_analysis_group",
          "type": "numeric",
          "description": "האם פעילויות הקבוצה לניתוח",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "activities_for_analysis_driver",
          "type": "numeric",
          "description": "האם פעילויות הנהג לניתוח",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "first_activity_for_group",
          "type": "numeric",
          "description": "האם מדובר בפעילות הראשונה של הקבוצה",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "last_activity_for_group",
          "type": "numeric",
          "description": "האם מדובר בפעילות האחרונה של הקבוצה",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "first_activity_of_the_day",
          "type": "numeric",
          "description": "האם מדובר בפעילות הראשונה ביום הפעילות",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "last_activity_of_the_day",
          "type": "numeric",
          "description": "האם מדובר בפעילות האחרונה ביום הפעילות",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "first_day_in_the_country",
          "type": "numeric",
          "description": "האם יום ראשון בארץ",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "last_day_in_the_country",
          "type": "numeric",
          "description": "האם יום אחרון בארץ",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "group_meeting_hour",
          "type": "character",
          "description": "שעת פגישת הקבוצה"
        },
        {
          "name": "group_separation_hour",
          "type": "character",
          "description": "שעת התפזרות הקבוצה"
        },
        {
          "name": "meeting_location",
          "type": "character",
          "description": "מקום מפגש",
          "vals": {
            "Ben Gurion Airport": null,
            "-1": null,
            "other": null
          }
        },
        {
          "name": "separation_location",
          "type": "character",
          "description": "מקום התפזרות",
          "vals": {
            "Ben Gurion Airport": null,
            "hotel": null,
            "Border Crossing": null
          }
        },
        {
          "name": "first_accommodation_activity",
          "type": "numeric",
          "description": "האם פעילות מגורים ראשונה",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "last_accommodation_activity",
          "type": "numeric",
          "description": "האם פעילות מגורים אחרונה",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "mid_day_accommodation_activity",
          "type": "numeric",
          "description": "האם פעילות מגורים של אמצע היום",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "trip_day_order",
          "type": "numeric",
          "description": "סדר יום הפעילות"
        },
        {
          "name": "trip_day_number_out_of_total_days",
          "type": "numeric",
          "description": "אחוז יום הפעילות"
        },
        {
          "name": "trip_day_portion",
          "type": "numeric",
          "description": "אחוז יום הפעילות מעוגל ל25 אחוזים"
        },
        {
          "name": "activity_weekday",
          "type": "character",
          "description": "יום הפעילות בשבוע",
          "vals": {
            "Sunday": null,
            "Monday": null,
            "Tuesday": null,
            "Wednesday": null,
            "Thursday": null,
            "Friday": null,
            "Saturday": null,
            "NA": null
          }
        },
        {
          "name": "Start_time_of_activity",
          "type": "numeric",
          "description": "שעת התחלת הפעילות בעיגול לשעות"
        },
        {
          "name": "Duration_of_activities_in_hours",
          "type": "difftime",
          "description": "אורך הפעילות בשעות בפורמט hh:mm:ss"
        },
        {
          "name": "number_of_activities_for_analysis_group",
          "type": "character",
          "description": "כמות הפעילויות היומיות לדיווח של הקבוצה",
          "comment": "במידה ומדובר בתיעוד קבוצה - תיעוד הנהג יכלול את המילים driver activity"
        },
        {
          "name": "number_of_activities_for_analysis_driver",
          "type": "character",
          "description": "כמות הפעילויות היומיות לדיווח של הנהג",
          "comment": "במידה ומדובר בתיעוד נהג- תיעוד הקבוצה  יכלול את המילים group"
        },
        {
          "name": "Number_of_tourist_areas_per_day",
          "type": "numeric",
          "description": "כמות אטרקציות תיוריות ליום"
        },
        {
          "name": "Arriving_Time_minutes",
          "type": "numeric",
          "description": "דקת הגעה"
        },
        {
          "name": "Arriving_Time_hour",
          "type": "numeric",
          "description": "שעת הגעה"
        },
        {
          "name": "Departure_Time_minutes",
          "type": "numeric",
          "description": "דקת יציאה"
        },
        {
          "name": "Departure_Time_hour",
          "type": "numeric",
          "description": "שעת יציאה"
        },
        {
          "name": "Drivers_traveling_duration_per_day",
          "type": "character",
          "description": "אורך זמן הנהיגה של הנהג ביום בפורמט hh:mm:ss",
          "comment": "במידה ומדובר בתיעוד קבוצה- השדה יכלול את המילים group activity"
        },
        {
          "name": "activities_in_Jerusalem",
          "type": "numeric",
          "description": "האם הפעילות התרחשה בירושלים",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "Jerusalem_day_trip",
          "type": "numeric",
          "description": "האם יום הפעילות כלל פעילות בירושלים",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "groups_and_Drivers_Jerusalem_trip_order",
          "type": "numeric",
          "description": "סדר הימים בירושלים של הנהג והקבוצה",
          "comment": "במידה ומדובר ביום בו הקבוצה עזבה את ירושלים ולא שהתה בה, הערך יהיה 0"
        },
        {
          "name": "morning_accommodation",
          "type": "character",
          "description": "מלל של מיקום המגורים בבוקר"
        },
        {
          "name": "evening_accommodation",
          "type": "character",
          "description": "מלל של מיקום המגורים בערב"
        },
        {
          "name": "identical_evening_and_morning_accommodation",
          "type": "numeric",
          "description": "האם השהייה במגורים בבוקר ובערב זהה",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "new_accommodation_location",
          "type": "character",
          "description": "מיקום המגורים בערב - קטגוריה כללית"
        },
        {
          "name": "area_of_accommodation_in_Jerusalem",
          "type": "character",
          "description": "מיקום המגורים בירושלים",
          "vals": {
            "NA": null,
            "The American Colony": null,
            "City entrance": null,
            "other": null,
            "city center": null,
            "Bethlehem": null,
            "Emek Refaim": null
          }
        },
        {
          "name": "traffic_Multi_zone",
          "type": "character",
          "description": "שם אזור על של הפעילות באנגלית"
        },
        {
          "name": "traffic_zone_number",
          "type": "character",
          "description": "מספר אזור התנועה של הפעילות"
        },
        {
          "name": "General_areas_of_the_country",
          "type": "character",
          "description": "אזור כללי בארץ של הפעילות",
          "vals": {
            "Central Israel": null,
            "North": null,
            "The Dead Sea": null,
            "Judaea and Samaria": null,
            "South": null,
            "Jerusalem and around": null,
            "Tel Aviv": null,
            "Jordan": null,
            "Not in Israel": null
          }
        },
        {
          "name": "tourist_final_area",
          "type": "character",
          "description": "שם אזור כללי של הפעילות"
        },
        {
          "name": "Tourist_sites_excluding_city",
          "type": "character",
          "description": "שם אטרציה תיירותית שאינה בעיר",
          "comment": "השדה ריק במידה והפעילות אינה באטרציה תיירותית מחוץ לעיר"
        },
        {
          "name": "factor",
          "type": "numeric",
          "description": "מקדם ניפוח של הקבוצה"
        },
        {
          "name": "Activity_distance_from_accommodation",
          "type": "numeric",
          "description": "מרחק הפעילות מהמדגורים בסוף היום"
        }
      ]
    },
    {
      "File name": "sail details.csv",
      "File format": "csv",
      "File description": "טבלת פרטים של קבוצות שיוט",
      "File fields": [
        {
          "name": "Record_Id",
          "type": "numeric",
          "description": "מזהה רשומה"
        },
        {
          "name": "travel_agent",
          "type": "character",
          "description": "סוכנות נסיעות מארגנית",
          "vals": {
            "Amiel": null,
            "Sarel": null,
            "Caspi": null
          }
        },
        {
          "name": "Group_name",
          "type": "character",
          "description": "שם הקבוצה",
          "comment": "מכילה את יעדי הקבוצה בישראל"
        },
        {
          "name": "activity_arrival_hour",
          "type": "difftime",
          "description": "שעת הגעה בפורמט hh:m:ss"
        },
        {
          "name": "departure_activity_Hour",
          "type": "difftime",
          "description": "שעת עזיבה בפורמט hh:m:ss"
        },
        {
          "name": "groups_meeting_place",
          "type": "character",
          "description": "מקום מפגש",
          "comment": "יש רק ערך יחודי אחד",
          "vals": {
            "Seaport": null
          }
        },
        {
          "name": "groups_departure_place",
          "type": "character",
          "description": "מקום עזיבה",
          "comment": "יש רק ערך יחודי אחד",
          "vals": {
            "Seaport": null
          }
        },
        {
          "name": "size_of_all_the_tour_groups",
          "type": "numeric",
          "description": "כמות האנשים בכלל הקבוצות"
        },
        {
          "name": "Individual_Group_size",
          "type": "numeric",
          "description": "כמות האנשים בקבוצה הנדגמת"
        },
        {
          "name": "Group_size_category",
          "type": "character",
          "description": "קטגורית גודל קבוצה",
          "vals": {
            "1-10": null,
            "10-20": null,
            "20-30": null,
            "30-40": null,
            "40-50": null
          }
        },
        {
          "name": "average_group_size_on_bus",
          "type": "numeric",
          "description": "כמות האנשים הממוצע באוטובוס",
          "vals": {
            "50+": null
          }
        },
        {
          "name": "number_of_Buses",
          "type": "numeric",
          "description": "כמות האוטבוסים של הקבוצה"
        },
        {
          "name": "Country_of_Origin",
          "type": "character",
          "description": "ארץ מוצא",
          "vals": {
            "United States": null,
            "Germany": null
          }
        },
        {
          "name": "Country_of_Origin_Category",
          "type": "character",
          "description": "יבשת מוצא",
          "vals": {
            "North America": null,
            "Europe": null
          }
        },
        {
          "name": "secondary_country_of_origin",
          "type": "character",
          "description": "ארץ מוצא שניונית",
          "comment": "קיים רק עבור קבוצה אחת",
          "vals": {
            "Switzerland": null
          }
        },
        {
          "name": "arrival_date",
          "type": "character",
          "description": "תאריך הגעה"
        },
        {
          "name": "departure_date",
          "type": "character",
          "description": "תאריך עזיבה"
        },
        {
          "name": "Number_days_of_stay",
          "type": "numeric",
          "description": "כמות ימים בהם הקבוצה שהתה בישראל",
          "comment": "קיים רק ערך אחד"
        },
        {
          "name": "length_of_stay",
          "type": "character",
          "description": "אורך הביקור - מלל",
          "comment": "קיים רק ערך אחד",
          "vals": {
            "short visit": null
          }
        },
        {
          "name": "month_of_arrival",
          "type": "numeric",
          "description": "חודש הגעה"
        },
        {
          "name": "Summer_Winter",
          "type": "character",
          "description": "קיץ או חורף?",
          "vals": {
            "Summer": null,
            "Winter": null
          }
        },
        {
          "name": "arrival_season",
          "type": "character",
          "description": "עונת הגעה",
          "vals": {
            "Tishrei 'Holidays": null,
            "Christmas": null,
            "Spring": null
          }
        },
        {
          "name": "Arrival_weekday",
          "type": "character",
          "description": "יום בשבוע בו הקבוצה הגיעה",
          "vals": {
            "Sunday": null,
            "Monday": null,
            "Tuesday": null,
            "Wednesday": null,
            "Thursday": null,
            "Friday": null,
            "Saturday": null
          }
        },
        {
          "name": "Departure_weekday",
          "type": "character",
          "description": "יום בשבוע בו הקבוצה עזבה",
          "vals": {
            "Sunday": null,
            "Monday": null,
            "Tuesday": null,
            "Wednesday": null,
            "Thursday": null,
            "Friday": null,
            "Saturday": null
          }
        },
        {
          "name": "number_of_activities_all_trip",
          "type": "numeric",
          "description": "כמות הפעיליות בכלל הביקור"
        },
        {
          "name": "average_number_of_activities_per_day",
          "type": "numeric",
          "description": "כמות ממוצעת של פעילויות ביום",
          "comment": "שווה לשדה הקודם"
        },
        {
          "name": "First_day_of_activity",
          "type": "date",
          "description": "תאריך יום הפעילות הראשון"
        },
        {
          "name": "Last_day_of_activity",
          "type": "date",
          "description": "תאריך יום הפעילות האחרון",
          "comment": "שווה לשדה הקודם"
        },
        {
          "name": "number_of_activity_days",
          "type": "numeric",
          "description": "כמות ימי הפעילות",
          "comment": "קיים רק ערך של יום אחד"
        },
        {
          "name": "activities_on_first_day",
          "type": "numeric",
          "description": "האם היו פעילויות ביום הראשון לביקור",
          "comment": "קיים רק ערך של יום אחד"
        },
        {
          "name": "activities_on_last_day",
          "type": "numeric",
          "description": "האם היו פעילויות ביום האחרון לביקור",
          "comment": "קיים רק ערך של יום אחד"
        },
        {
          "name": "didnt_sleep_in_Jerusalem",
          "type": "numeric",
          "description": "האם הקבוצה לא ישנה בירושלים",
          "comment": "בקבוצות בהם הערך הוא 0 הדגימה של הקבוצה הייתה ליום אחד בלבד. יתכן ומדובר באותה הקבוצה (מזהה 23309 ו-128624. יתכן שגם מזהה 128625 שייך לה)"
        },
        {
          "name": "number_of_accommodations",
          "type": "numeric",
          "description": "כמות מקומות הלינה",
          "comment": "קיים רק ערך אחד"
        },
        {
          "name": "average_nights_at_accommodation",
          "type": "numeric",
          "description": "ממוצע הלילות למקום לינה",
          "comment": "קיים רק ערך אחד"
        },
        {
          "name": "number_of_days_in_Jerusalem",
          "type": "numeric",
          "description": "כמות ימים בירושלים"
        },
        {
          "name": "factor",
          "type": "numeric",
          "description": "מקדם ניפוח"
        },
        {
          "name": "Group_Name_heb",
          "type": "character",
          "description": "שם קבוצה עברית",
          "comment": "מתאר את האזור בו הקבוצה הסתובבה"
        },
        {
          "name": "Group_Name_eng",
          "type": "character",
          "description": "שם קבוצה אנגלית",
          "comment": "מתאר את האזור בו הקבוצה הסתובבה"
        }
      ]
    },
    {
      "File name": "sail activities.csv",
      "File format": "csv",
      "File description": "טבלת פעילויות של קבוצות שיוט",
      "File fields": [
        {
          "name": "Record_ID",
          "type": "numeric",
          "description": "מזהה רשומה"
        },
        {
          "name": "Member_ID",
          "type": "numeric",
          "description": "מזהה חבר קבוצה"
        },
        {
          "name": "Activity_ID",
          "type": "numeric",
          "description": "מזהה פעילות"
        },
        {
          "name": "Travel_Day",
          "type": "character",
          "description": "תאריך הפעילות"
        },
        {
          "name": "Place_Number",
          "type": "numeric",
          "description": "מספר הפעילות"
        },
        {
          "name": "Activity_Name",
          "type": "character",
          "description": "שם מקום הפעילות"
        },
        {
          "name": "Arriving_Time",
          "type": "difftime",
          "description": "שעת הגעה"
        },
        {
          "name": "Departure_Time",
          "type": "difftime",
          "description": "שעת עזיבה"
        },
        {
          "name": "Original_Arriving_Time",
          "type": "difftime",
          "description": "שעת הגעה לא ערוכה"
        },
        {
          "name": "Original_Departure_Time",
          "type": "difftime",
          "description": "שעת עזיבה לא ערוכה"
        },
        {
          "name": "TAZ_2640",
          "type": "numeric",
          "description": "מספר אזור התנועה בשכבת 2640 אזורי תנועה בו בוצעה הפעילות"
        },
        {
          "name": "Purpose_of_activity",
          "type": "character",
          "description": "מטרת הפעילות",
          "vals": {
            "Group arrival / departure": null,
            "Shopping / Arrangements / Food": null,
            "Tourism / leisure / religious services / sports activities": null,
            "Changing means of transportation": null,
            "Hotel / Holiday house / Residence": null,
            "parking": null,
            "pick up/let off passenger": null,
            "Parking in the bus parking lot": null,
            "Driver Personal Activity": null,
            "Gas station / maintenance of the bus": null
          }
        },
        {
          "name": "means_of_transport",
          "type": "character",
          "description": "אמצעי הגעה לפעילות",
          "vals": {
            "NA": null,
            "The group bus": null,
            "walking": null,
            "other": null
          }
        },
        {
          "name": "comments",
          "type": "character",
          "description": "הערות לגבי קליטת הפעילות"
        },
        {
          "name": "activity_order_in_file",
          "type": "numeric",
          "description": "פעילות לפי סדר כרונולוגי בתוך כלל הפעילויות"
        },
        {
          "name": "record_ID_plus_Member_Id",
          "type": "numeric",
          "description": "שילוב של מזהה הקבוצה ומזהה חבר הקבוצה"
        },
        {
          "name": "Record_Id_plus_travel_day",
          "type": "numeric",
          "description": "שילוב של מזהה הקבוצה ומזהה כמות ימים מהתאריך הראשון באקסל"
        },
        {
          "name": "ID_plus_MEMBER_plus_DAY",
          "type": "numeric",
          "description": "שילוב של מזהה הקבוה, חבר הקבוצה ומזהה כמות ימים"
        },
        {
          "name": "Group_category",
          "type": "character",
          "description": null,
          "comment": "קיים רק ערך אחד",
          "vals": {
            "Cruise groups": null
          }
        },
        {
          "name": "Name_of_cruise_group",
          "type": "character",
          "description": "שם קבוצת השיוט"
        },
        {
          "name": "group_size_category",
          "type": "character",
          "description": "קטגורית גודל קבוצה",
          "vals": {
            "1-10": null,
            "10-20": null,
            "20-30": null,
            "30-40": null,
            "40-50": null,
            "50+": null
          }
        },
        {
          "name": "number_days_of_stay",
          "type": "numeric",
          "description": "כמות ימי שהייה",
          "comment": "קיים רק ערך אחד. יתכן ולא מדויק",
          "vals": {
            "1": null
          }
        },
        {
          "name": "activities_for_analysis_group",
          "type": "numeric",
          "description": "האם הפעילות לניתוח של הקבוצה",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "activities_for_analysis_driver",
          "type": "numeric",
          "description": "האם הפעילות לניתוח של הנהג",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "first_activity_for_group",
          "type": "numeric",
          "description": "האם פעילות קבוצה ראשונה",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "last_activity_for_group",
          "type": "numeric",
          "description": "האם פעילות קבוצה אחרונה",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "first_activity_of_the_day",
          "type": "numeric",
          "description": "האם פעילות יומית ראשונה",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "last_activity_of_the_day",
          "type": "numeric",
          "description": "האם פעילות יומית אחרונה",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "first_day_in_the_country",
          "type": "numeric",
          "description": "האם יום ראשון בישראל",
          "comment": "קיים רק ערך אחד"
        },
        {
          "name": "last_day_in_the_country",
          "type": "numeric",
          "description": "האם יום אחרון בישראל",
          "comment": "קיים רק ערך אחד"
        },
        {
          "name": "group_meeting_hour",
          "type": "numeric",
          "description": "שעת הפגישה של הקבוצה"
        },
        {
          "name": "group_separation_hour",
          "type": "numeric",
          "description": "שעת ההתפזרות של הקבוצה"
        },
        {
          "name": "meeting_location",
          "type": "character",
          "description": "מיקום המפגש",
          "vals": {
            "Ashdod port": null,
            "Port of Haifa": null,
            "other": null
          }
        },
        {
          "name": "separation_location",
          "type": "character",
          "description": "מיקום ההתפזרות",
          "vals": {
            "Hotel": null,
            "port": null,
            "Port of Haifa": null
          }
        },
        {
          "name": "first_accommodation_activity",
          "type": "numeric",
          "description": "האם פעילות מגורים ראשונה",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "last_accommodation_activity",
          "type": "numeric",
          "description": "האם פעילות מגורים אחרונה",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "mid_day_accommodation_activity",
          "type": "numeric",
          "description": "האם פעילות מגורים באמצע היום",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "trip_day_order",
          "type": "numeric",
          "description": "סדר יום הפעילות",
          "comment": "קיים רק ערך אחד"
        },
        {
          "name": "trip_day_number_out_of_total_days",
          "type": "numeric",
          "description": "מיקום יום הפעילות מכלל הימים באחוזים",
          "comment": "קיים רק ערך אחד"
        },
        {
          "name": "trip_day_portion",
          "type": "numeric",
          "description": "מיקום יום הפעילות מכלל הימים ברבעים",
          "comment": "קיים רק ערך אחד"
        },
        {
          "name": "activity_weekday",
          "type": "character",
          "description": "יום הפעילות בשבוע",
          "vals": {
            "Sunday": null,
            "Monday": null,
            "Tuesday": null,
            "Wednesday": null,
            "Thursday": null,
            "Friday": null,
            "Saturday": null
          }
        },
        {
          "name": "arrival_season",
          "type": "character",
          "description": "עונת ההגעה",
          "vals": {
            "Tishrei 'Holidays": null,
            "Christmas": null,
            "Spring": null
          }
        },
        {
          "name": "Start_time_of_activity",
          "type": "numeric",
          "description": "שעתהתחלת הפעילות"
        },
        {
          "name": "Duration_of_activities_in_hours",
          "type": "numeric",
          "description": "אורך הפעילות כאחוז של אורך היום",
          "comment": "יש להכפיל ב-24 על מנת לקבל את כמות השעות"
        },
        {
          "name": "number_of_activities_A_day_per_group_for_analysis",
          "type": "character",
          "description": "כמות הפעילות של הקבוצה לניתוח. במידה ומדובר בפעילות נהג - מצוין פעילות נהג"
        },
        {
          "name": "number_of_Drivers_activities_A_day_for_analysis",
          "type": "character",
          "description": "כמות הפעילות של הנהג לניתוח. במידה ומדובר בפעילות קבוצה - מצוין קבוצה"
        },
        {
          "name": "Number_of_tourist_areas_per_day",
          "type": "numeric",
          "description": "כמות אזורי תיירות ליום בהם הקבוצה שהתה"
        },
        {
          "name": "Arriving_Time_minutes",
          "type": "numeric",
          "description": "דקת הגעה לפעילות"
        },
        {
          "name": "Arriving_Time_hour",
          "type": "numeric",
          "description": "שעת הגעה לפעילות"
        },
        {
          "name": "Departure_Time_minutes",
          "type": "numeric",
          "description": "דקת עזיבת הפעילות"
        },
        {
          "name": "Departure_Time_hour",
          "type": "numeric",
          "description": "שעת עזיבת הפעילות"
        },
        {
          "name": "activities_in_Jerusalem",
          "type": "numeric",
          "description": "האם הפעילות בירושלים",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "Jerusalem_day_trip",
          "type": "numeric",
          "description": "האם היו באותו יום פעילויות בירושלים",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "groups_and_Drivers_Jerusalem_trip_order",
          "type": "numeric",
          "description": "לא ברור",
          "vals": {
            "0": "לא",
            "1": "כן"
          }
        },
        {
          "name": "morning_accommodation",
          "type": "character",
          "description": "מיקום המגורים בבוקר",
          "vals": {
            "First day in Israel": null,
            "NA": null,
            "Ashdod": null,
            "Jerusalem": null,
            "Haifa": null
          }
        },
        {
          "name": "evening_accommodation",
          "type": "character",
          "description": "מיקום המגורים בערב",
          "vals": {
            "Jerusalem": null,
            "NA": null,
            "Last day in Israel": null,
            "Haifa": null,
            "Ashdod": null
          }
        },
        {
          "name": "identical_evening_and_morning_accommodation",
          "type": "numeric",
          "description": "האם מיקום המגורים בבוקר ובערב שווה"
        },
        {
          "name": "area_of_accommodation_in_Jerusalem",
          "type": "character",
          "description": "איזור המגורים היומי בירושלים"
        },
        {
          "name": "traffic_Multi_zone",
          "type": "character",
          "description": "שם של אזור תנועה כללי בו התבצעה הפעילות"
        },
        {
          "name": "traffic_zone_number",
          "type": "character",
          "description": "מספר אזור תנועה בו התבצעה הפעילות"
        },
        {
          "name": "Municipal_Area_layer",
          "type": "character",
          "description": "שם רשות מקומית בה התבצעה הפעילות"
        },
        {
          "name": "country_area",
          "type": "character",
          "description": "איזור הפעילות בארץ",
          "vals": {
            "Central Israel": null,
            "Jerusalem and around": null,
            "North": null,
            "Judaea and Samaria": null,
            "South": null,
            "The Dead Sea": null,
            "Tel Aviv": null
          }
        },
        {
          "name": "visiting_area",
          "type": "character",
          "description": null
        },
        {
          "name": "Tourist_sites_excluding_cities"
        },
        {
          "name": "factor",
          "type": "numeric",
          "description": "מקדם ניפוח"
        },
        {
          "name": "trip_name",
          "type": "character",
          "description": "שם הסיור - גרסה 1",
          "vals": {
            "Jerusalem &Bethlehem": null,
            "north": null,
            "other": null
          }
        },
        {
          "name": "trip_name_2",
          "type": "character",
          "description": "שם הסיור - גרסה 2",
          "vals": {
            "Jerusalem &Bethlehem": null,
            "north": null,
            "The Dead Sea & the South": null,
            "Tel Aviv": null
          }
        },
        {
          "name": "activity_start_time",
          "type": "duration",
          "description": "שעת התחלת הפעילות בפורמט hh:mm:ss"
        },
        {
          "name": "activity_end_time",
          "type": "duration",
          "description": "שעת סיום הפעילות בפורמט hh:mm:ss"
        },
        {
          "name": "Place_of_arrival",
          "type": "character",
          "description": "מקום ההגעה הראשוני של הקבוצה",
          "vals": {
            "Ashdod port": null,
            "Haifa Port": null,
            "Eilat": null,
            "4": null
          }
        }
      ]
    },
    {
      "File name": "taz_arzi_2640.shp",
      "File format": "shapefile",
      "File description": "שכבת אזורי תנועה (מקור: מודל ארצי גרסה 4/20, תיקון לטובת הסקר)",
      "File Comments": "השכבה מבוססת על שכבת האזורים ששימשה את הקובץ האחוד לסקרי הרגלי נסיעה, בתוספת אזורים לייצוג נסיעות תיירים בחו\"ל, כולל 3 אזורים בירדן (מספר 8001 עד 8003) ואזור אחד במייצג את סיני ומצריים (מספר 9001)",
      "File fields": [
        {
          "name": "TAZ_2640",
          "type": "Integer",
          "description": "אזור תנועה"
        },
        {
          "name": "Desc",
          "type": "Character",
          "description": "תיאור האזור"
        },
        {
          "name": "MetroLMS",
          "type": "Integer",
          "description": "קוד מטרופולין למ\"ס"
        },
        {
          "name": "RingLMS",
          "type": "Integer",
          "description": "טבעת מטרופולין למ\"ס",
          "comment": "גזרת מטרופולין",
          "vals": {
            "110": "ת\"א גלעין",
            "121": "ת\"א טב' פנימית גז' צפונית",
            "122": "ת\"א טב' פנימית  גז' מזרחית",
            "123": "ת\"א טב' פנימית  גז' דרומית",
            "131": "ת\"א טב' תיכונה גז' צפונית",
            "132": "ת\"א טב' תיכונה גז' מזרחית",
            "133": "ת\"א טב' תיכונה גז' דרומית",
            "141": "ת\"א טב' חיצונית גז' צפונית",
            "142": "ת\"א טב' חיצונית גז' מזרחית",
            "143": "ת\"א טב' חיצונית גז' דרומית",
            "145": "ת\"א יישובים ישראליים באיו\"ש",
            "210": "חיפה גלעין",
            "221": "חיפה טב' פנימית גז' צפונית",
            "222": "חיפה טב' פנימית  גז' מזרחית",
            "223": "חיפה טב' פנימית גז' דרומית",
            "241": "חיפה טב' חיצונית גז' צפונית",
            "242": "חיפה טב' חיצונית גז' מזרחית",
            "243": "חיפה טב' חיצונית גז' דרומית",
            "310": "ב\"ש גלעין",
            "341": "ב\"ש טב' חיצונית גז' צפונית",
            "342": "ב\"ש טב' חיצונית גז' מזרחית",
            "344": "ב\"ש טב' חיצונית גז' מערבית",
            "410": "ירושלים גלעין",
            "444": "ירושלים טב' חיצונית גז' מערבית",
            "445": "ירושלים יישובים ישראליים באיו\"ש"
          }
        },
        {
          "name": "TAZ_1250",
          "type": "Integer",
          "description": "שיוך חלוקה ל-1250 א\"ת",
          "comment": "ע\"פ הגדרות המודל הארצי והתאמות לסקר תיירים (ירדן, מצרים)"
        },
        {
          "name": "TAZ_250",
          "type": "Integer",
          "description": "שיוך חלוקה ל-250 א\"ת",
          "comment": "ע\"פ הגדרות המודל הארצי והתאמות לסקר תיירים (ירדן, מצרים)"
        },
        {
          "name": "TAZ_33",
          "type": "Integer",
          "description": "שיוך חלוקה ל-33 א\"ת",
          "comment": "ע\"פ הגדרות המודל הארצי והתאמות לסקר תיירים (ירדן, מצרים)"
        },
        {
          "name": "Area_sqm",
          "type": "Integer",
          "description": "שטח",
          "comment": "מ\"ר"
        }
      ]
    }
  ]
}
;
